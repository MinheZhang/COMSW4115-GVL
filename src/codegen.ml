module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
	let context = L.global_context () in
	(* Create the LLVM compilation module into which
	we will generate code *)
	let the_module = L.create_module context "GVL" in

	let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context in

	let ltype_of_typ = function
	    A.Int -> i32_t
	in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
				Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

	let build_function_body fdecl =
	  (* TODO *)
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals":
        1. formal argurments,
        2. locally decalred variables.
      Allocate each on the stack, initialize their value,
      if appropriate, and remember their values in the "locals" map
    *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          StringMap.add n local m 
  
          (* Allocate space for any locally declared variables and add the
          * resulting registers to our map *)
          and add_local m (t, n) =
            let local_var = L.build_alloca (ltype_of_typ t) n builder
            in StringMap.add n local_var m 
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
            (Array.to_list (L.params the_function)) 
      in
        List.fold_left add_local formals fdecl.slocals 
    in
    (* Return the value for a variable or formal argument.
        Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                    with Not_found -> StringMap.find n global_vars
    in
    
	  let rec expr builder ((_, e) : sexpr) = match e with
	      SBinop (e1, op, e2) ->
		      let e1' = expr builder e1
		      and e2' = expr builder e2 in
		      (match op with
					  A.Add     -> L.build_add
					| A.Sub     -> L.build_sub
					| A.Mul     -> L.build_mul
				  | A.Div     -> L.build_sdiv
				  | A.Mod     -> L.build_srem
					| A.And     -> L.build_and
					| A.Or      -> L.build_or
					| A.Equal   -> L.build_icmp L.Icmp.Eq
					| A.Neq     -> L.build_icmp L.Icmp.Ne
					| A.Less    -> L.build_icmp L.Icmp.Slt
					| A.Leq     -> L.build_icmp L.Icmp.Sle
					| A.Greater -> L.build_icmp L.Icmp.Sgt
					| A.Geq     -> L.build_icmp L.Icmp.Sge
		      ) e1' e2' "tmp" builder
	    | SUnop(op, e) ->
	        let e' = expr builder e in
	        (match op with
	          A.Neg -> L.build_neg
	        | A.Not -> L.build_not) e' "tmp" builder
	    
	    | SAssign (v, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup v) builder); e'
	    (* | SCall (f, args) -> raise (Failure "codegen expr scall not implemented") *)
	    | SId s -> L.build_load (lookup s) s builder
	    | SIntLit i -> L.const_int i32_t i
	  in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
				Some _ -> ()
      | None -> ignore (instr builder) in

    let rec stmt builder = function
				SBlock sl ->  List.fold_left stmt builder sl
      | SExpr e ->    ignore(expr builder e); builder
      | SVdecl b -> builder (* TODO *)
      | SReturn e ->  ignore (L.build_ret (expr builder e) builder);
                      builder
    in

    let builder = stmt builder (SBlock fdecl.sbody) in

    add_terminal builder (L.build_ret (L.const_int (ltype_of_typ fdecl.styp) 0))
	(* ... *)
	in
	List.iter build_function_body functions;
	the_module
