open Ast

type sexpr = typ * sx
and sx =
    SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SId of string
  | SIntLit of int

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SVdecl of bind
  | SReturn of sexpr

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals: bind list;
    sbody : sstmt list;
  }

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
      SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SUnop(o, e) -> string_of_uop o ^ " " ^ string_of_sexpr e
    | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
    | SCall(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    | SId(s) -> s
    | SIntLit(l) -> string_of_int l
                  ) ^ ")"

let rec string_of_sstmt = function
    SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SVdecl(b) -> string_of_bind b
  | _ -> raise (Failure ("sast stmt not implemented"))

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  (* String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^ *)
  String.concat "\n" (List.map string_of_sfdecl funcs)    
