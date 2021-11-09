type op = Add | Sub | Mul | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Bool | Int | Float | Char | String | StructID | Node | Edge | Graph

type bind = typ * string

type expr = 
    Binop of expr * op * expr
|   Unop of uop * expr
|   Assign of string * expr
|   Id of string
|   IntLit of int
    (* Function Call *)
|   Call of string * expr list
|   Noexpr

type stmt = 
    Expr of expr
|   Vdecl of bind
|   Block of stmt list
|   If of expr * stmt * stmt
|   For of expr * expr * expr * stmt
|   While of expr * stmt
|   Break
|   Continue
|   Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    (*locals : bind list;*) (*TODO*)
    body : stmt list;
  }

type program = bind list * func_decl list

let string_of_op = function
    Add -> "+"
|   Sub -> "-"
|   Mul -> "*"
|   Div -> "/"
|   Equal -> "=="
|   Neq -> "!="
|   Less -> "<"
|   Leq -> "<="
|   Greater -> ">"
|   Geq -> ">="
|   And -> "&&"
|   Or -> "||"

let string_of_uop = function
    Neg -> "-"
|   Not -> "!"

let string_of_typ = function
    Bool -> "bool"
|   Int -> "int"
|   Float -> "float"
|   Char -> "char"
|   StructID -> "struct" (* *)
|   Node -> "node"
|   Edge -> "edge"
|   Graph -> "graph"
    (* typ LBRACKET RBRACKET *)

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" 

let rec string_of_expr = function
    Binop(e1, o, e2) -> 
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
|   Unop(o, e) -> string_of_uop o ^ string_of_expr e
|   Assign(v, e) -> v ^ " = " ^ string_of_expr e
|   Id(s) -> s
|   IntLit(l) -> string_of_int l
    (* Function *)

let rec string_of_stmt = function
    Expr(e) -> string_of_expr e ^ ";\n"
(*|   If(e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s*)
|   Vdecl(b) -> string_of_bind b
|   For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
|   While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
|   Return(e) -> "return " ^ string_of_expr e ^ ";\n"
(* Break *)
(* Continue *)

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* string_of_fdecl *)
let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  (*String.concat "" (List.map string_of_vdecl fdecl.locals) ^*)(*TODO*)
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(* string_of_program *)
let string_of_program (vars, funcs) =
  (*String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^*) (*TODO*)
  String.concat "\n" (List.map string_of_fdecl funcs)