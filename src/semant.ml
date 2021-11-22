open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, functions) =
	let check_function func =

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals)
    in

	  (* Return a variable from local symbol table *)
	  (* let type_of_identifier s =
	    try StringMap.find s symbols
	    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
	  in *)
    let type_of_identifier s locals =
	    try StringMap.find s symbols
	    with Not_found -> 
        try StringMap.find s locals
        with Not_found -> 
          raise (Failure ("undeclared identifier " ^ s))
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
		      | Equal | Neq           when same              -> Bool
		      | Less | Leq | Greater | Geq
		          when same && (t1 = Int)                    -> Bool
		      | And | Or              when same && t1 = Bool -> Bool
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
      | Call(fname, args) as call -> (* TODO *)
          raise (Failure ("semant expr Call not implemented"))
		  | Id s -> (type_of_identifier s locals, SId s)
		  | IntLit l -> (Int, SIntLit l)
		in

		(* TODO stmt *)
		let rec check_stmt (sstmt_list, locals) stmt = match stmt with
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
		  | _ -> raise (Failure ("semant stmt not implemented"))
	  in

    let slocals_map_to_list n t slocals_list =
      (t, n) :: slocals_list
    in

    let (sbody, slocals_map) = List.fold_left check_stmt ([], StringMap.empty) func.body (* TODO *)
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