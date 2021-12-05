module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
	let context = L.global_context () in
  let llmem = L.MemoryBuffer.of_file "graph" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in

	(* Create the LLVM compilation module into which
	we will generate code *)
	let the_module = L.create_module context "GVL" in

	let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and node_t     = L.pointer_type (match L.type_by_name llm "struct.node_t" with
                                              None -> raise (Failure "the node type is not defined.")
                                            | Some x -> x)
  and edge_t     = L.pointer_type (match L.type_by_name llm "struct.edge_t" with
                                              None -> raise (Failure "the edge type is not defined.")
                                            | Some x -> x)
  and void_ptr_t = L.pointer_type (L.i8_type context)
  in

	let ltype_of_typ = function
	    A.Int -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.VoidPtr -> void_ptr_t
    | A.Node -> node_t
    | A.Edge -> edge_t
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

  (* Node functions *)
  let create_node_t : L.lltype = 
    L.function_type node_t [| float_t; float_t; float_t; i32_t; i32_t; i32_t; void_ptr_t |] in
  let create_node_func : L.llvalue =
    L.declare_function "create_node" create_node_t the_module in
  (* Edge functions *)
  let create_edge_t : L.lltype =
    L.function_type edge_t [| node_t ; node_t ; i32_t ; i32_t ; i32_t ; i32_t |] in
  let create_edge_func : L.llvalue =
    L.declare_function "create_edge" create_edge_t the_module in  

  (* built-in functions *)
  let built_in_func_decls : L.llvalue StringMap.t =
    let f map (name, func) = StringMap.add name func map in
    List.fold_left f StringMap.empty [("create_edge", create_edge_func)]
  in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : L.llvalue StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module) m in
    List.fold_left function_decl built_in_func_decls functions in
  (*
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
				Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  *)

	let build_function_body fdecl =
	  (* TODO *)
    (* let (the_function, _) = StringMap.find fdecl.sfname function_decls in *)
    let the_function = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

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
        SBinop ((A.Float, _) as e1, op, e2) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
          (match op with 
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mul     -> L.build_fmul
          | A.Div     -> L.build_fdiv 
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.Mod | A.And | A.Or ->
              raise (Failure "internal error: semant should have rejected mod/and/or on float")
          ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
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
	    (* Function call *)
      | SCall ("printi", [e]) | SCall ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder e) |]
            "printf" builder
      | SCall ("printf", [e]) ->
          L.build_call printf_func [| float_format_str ; (expr builder e) |]
            "printf" builder
      | SCall ("printc", [e]) ->
          L.build_call printf_func [| char_format_str ; (expr builder e) |]
            "printf" builder
      | SCall ("prints", [e]) ->
          L.build_call printf_func [| string_format_str ; (expr builder e) |]
            "printf" builder
      (* TODO void pointer *)
      | SCall ("create_node", [x; y; radius; r; g; b; data]) ->
        let x'      = (expr builder x)
        and y'      = (expr builder y)
        and radius' = (expr builder radius)
        and r'      = (expr builder r)
        and g'      = (expr builder g)
        and b'      = (expr builder b) in
        L.build_call create_node_func [| x'; y'; radius'; r'; g'; b'; (L.const_pointer_null void_ptr_t) |]
          "create_node" builder
      | SCall (f, args) ->
          (* let (fdef, fdecl) = StringMap.find f function_decls in *)
          let fdef = StringMap.find f function_decls in
          let llargs = List.rev (List.map (expr builder) (List.rev args)) in
          L.build_call fdef (Array.of_list llargs) (f ^ "_result") builder
      (* Id *)
	    | SId s -> L.build_load (lookup s) s builder
      (* Lit *)
	    | SIntLit i   -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit l -> L.const_float_of_string float_t l
      | SCharLit c  -> L.const_int i8_t (Char.code c)
      | SStrLit s   -> L.build_global_stringptr s "fmt" builder
      | SNoexpr     -> L.const_int i32_t 0
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
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
    let merge_bb = L.append_block context "merge" the_function in
          let build_br_merge = L.build_br merge_bb in (* partial function *)

    let then_bb = L.append_block context "then" the_function in
    add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
      build_br_merge;

    let else_bb = L.append_block context "else" the_function in
    add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
      build_br_merge;

    ignore(L.build_cond_br bool_val then_bb else_bb builder);
    L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore(L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
               


    in

    let builder = stmt builder (SBlock fdecl.sbody) in

    add_terminal builder (L.build_ret (L.const_int (ltype_of_typ fdecl.styp) 0))
	(* ... *)
	in
	List.iter build_function_body functions;
	the_module
