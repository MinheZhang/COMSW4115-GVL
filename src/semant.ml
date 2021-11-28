open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, functions) =
	(* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Int;
      fname = name; 
      formals = [(ty, "x")];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("printi", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
			                         ("printc", Char);
			                         ("prints", String) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "redefinition of built-in function '" ^ fd.fname ^ "'"
    and dup_err = "redefinition of function '" ^ fd.fname ^ "'"
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

	let check_function func =
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals)
    in

    (* let find_in_locals s' locals' = match locals' with
      l :: ls -> try StringMap.find s' l
                  with Not_found -> find_in_locals s' ls *)
	  (* Return a variable from local symbol table *)
	  (* let type_of_identifier s =
	    try StringMap.find s symbols
	    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
	  in *)
    let type_of_identifier s locals =
	    try StringMap.find s symbols
	    with Not_found ->
        let rec find_in_locals s' locals' =
          match locals' with
            [] -> raise (Failure ("undeclared identifier " ^ s'))
          | l::ls -> try StringMap.find s' l
                      with Not_found -> find_in_locals s' ls
        in find_in_locals s locals
	  in

		(* Return a semantically-checked expression, i.e., with a type *)
		let rec check_expr expr locals = match expr with
		    Binop(e1, op, e2) as ex ->
		      let (t1, e1') = check_expr e1 locals
		      and (t2, e2') = check_expr e2 locals in
		      (* All binary operators require operands of the same type *)
		      let same = (t1 = t2) in
		      (* TODO other op & types e.g. float/bool/char/string/node/graph *)
		      let ty = match op with
		        Add | Sub | Mul | Div | Mod
		          when same && t1 = Int  -> Int
					| Add | Sub | Mul | Div when same && t1 = Float 	-> Float
		      | Equal | Neq           when same              		-> Bool
		      | Less | Leq | Greater | Geq
		          when same && (t1 = Int || t1 = Float)         -> Bool
		      | And | Or              when same && t1 = Bool 		-> Bool
		      | _ -> raise (
		           Failure ("illegal binary operator " ^
	                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
	                       string_of_typ t2 ^ " in " ^ string_of_expr ex))
		      in (ty, SBinop((t1, e1'), op, (t2, e2')))
		  | Unop(op, e) as ex ->
		      let (t, e') = check_expr e locals in
		      let ty = match op with
		      | Neg when t = Int -> t (* TODO Float *)
		      | Not when t = Bool -> t
		      | _ -> raise (Failure ("illegal unary operator " ^ 
	                                string_of_uop op ^ string_of_typ t ^
	                                " in " ^ string_of_expr ex))
		      in (ty, SUnop(op, (t, e')))
		  | Assign(v, e) as ex ->
		      let v_t = type_of_identifier v locals
		      and (e_t, e') = check_expr e locals in
		      let err = "illegal assignment " ^ string_of_typ v_t ^ " = " ^ 
            string_of_typ e_t ^ " in " ^ string_of_expr ex in
          let ty = if v_t = e_t then v_t else raise (Failure err) in
          (ty, SAssign(v, (e_t, e')))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = check_expr e locals in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
      | Id s         	-> (type_of_identifier s locals, SId s)
      | IntLit l      -> (Int, SIntLit l)
      | BoolLit l     -> (Bool, SBoolLit l)
      | FloatLit l    -> (Float, SFloatLit l)
      | CharLit l     -> (Char, SCharLit l)
      | StrLit l      -> (String, SStrLit l)
      | Noexpr        -> (Int, SNoexpr)
    in

    let check_bool_expr e locals = 
      let (t', e') = check_expr e locals
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

		(* TODO stmt *)
		(* let rec check_stmt (sstmt_list, locals) stmt = match stmt with
		    Expr e -> (SExpr (check_expr e locals) :: sstmt_list, locals)
		  | Vdecl (t, n) -> 
        if StringMap.mem n symbols 
          then raise (Failure ("redefinition of " ^ "'" ^ n ^ "'"))
          else if StringMap.mem n locals
                then raise (Failure ("redefinition of " ^ "'" ^ n ^ "'"))
                else (SVdecl (t, n) :: sstmt_list, StringMap.add n t locals)
      | Return e -> let (t, e') = check_expr e locals in
        if t = func.typ then (SReturn (t, e') :: sstmt_list, locals)
        else raise (Failure ("return gives " ^ string_of_typ t 
                              ^ " expected " ^ string_of_typ func.typ 
                              ^ " in " ^ string_of_expr e))
		  | _ -> raise (Failure ("semant stmt not implemented")) *)
    let rec check_stmt (sstmt_list, all_locals, curr_locals) stmt = match stmt with
        Expr e -> (SExpr (check_expr e curr_locals) :: sstmt_list, all_locals, curr_locals)
      | Vdecl (t, n) ->
        if StringMap.mem n symbols
          then raise (Failure ("redefinition of " ^ "'" ^ n ^ "'"))
          else if StringMap.mem n all_locals
                then raise (Failure ("redefinition of " ^ "'" ^ n ^ "'"))
                else (SVdecl (t, n) :: sstmt_list, 
                      StringMap.add n t all_locals,
                      (StringMap.add n t (List.hd curr_locals)) :: List.tl curr_locals)
      | Return e -> let (t, e') = check_expr e curr_locals in
        if t = func.typ then (SReturn (t, e') :: sstmt_list, all_locals, curr_locals)
        else raise (Failure ("return gives " ^ string_of_typ t
                              ^ " expected " ^ string_of_typ func.typ 
                              ^ " in " ^ string_of_expr e))
      | Block sl ->
          let rec check_stmt_list = function
              Return _ :: _ -> raise (Failure "nothing may follow a return")
            | s :: ss -> check_stmt_list ss
            | [] -> () in
          check_stmt_list sl;
          let (blist, new_all_locals, _) =
            List.fold_left check_stmt ([], all_locals, StringMap.empty :: curr_locals) sl
          in
          (SBlock(List.rev blist)::sstmt_list, new_all_locals, curr_locals)
      | If(p, b1, b2) ->
          let sp = check_bool_expr p curr_locals in
          let (b1_sstmt_list, b1_all_locals, _) = check_stmt ([], all_locals, curr_locals) b1 in
          let (b2_sstmt_list, b2_all_locals, _) = check_stmt ([], b1_all_locals, curr_locals) b2 in
          (SIf(sp, List.hd b1_sstmt_list, List.hd b2_sstmt_list) :: sstmt_list, b2_all_locals, curr_locals)
      | For(e1, e2, e3, st) ->
        let (for_sstmt, new_all_locals, _) = check_stmt ([], all_locals, curr_locals) st in
        (SFor(check_expr e1 curr_locals,
              check_bool_expr e2 curr_locals,
              check_expr e3 curr_locals,
              List.hd for_sstmt) :: sstmt_list,
          new_all_locals,
          curr_locals)
      | While(p, s) -> 
          let (while_sstmt, new_all_locals, _) = check_stmt ([], all_locals, curr_locals) s in
          (SWhile(check_bool_expr p curr_locals, List.hd while_sstmt) :: sstmt_list,
            new_all_locals,
            curr_locals)
      | _ -> raise (Failure ("semant stmt not implemented"))

	  in

    let slocals_map_to_list n t slocals_list =
      (t, n) :: slocals_list
    in

    let (sbody, slocals_map, _) = List.fold_left check_stmt ([], StringMap.empty, [StringMap.empty]) func.body (* TODO *)
    in
		{ styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      (* slocals  = func.locals; *)
      (* TODO: reverse list. *)
      sbody  = List.rev sbody;
      slocals = StringMap.fold slocals_map_to_list slocals_map []
    }
	in (globals, List.map check_function functions)