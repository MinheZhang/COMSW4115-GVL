type token =
  | EQ
  | NEQ
  | GEQ
  | LEQ
  | GT
  | LT
  | AND
  | OR
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | TIMES_ASSIGN
  | DIVIDE_ASSIGN
  | MOD_ASSIGN
  | IF
  | ELSE
  | WHILE
  | FOR
  | BREAK
  | CONTINUE
  | RETURN
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | SEMI
  | BOOL
  | INT
  | FLOAT
  | CHAR
  | STRING
  | STRUCT
  | NODE
  | EDGE
  | GRAPH
  | DOT
  | TRUE
  | FALSE
  | INTLIT of (int)
  | FLOATLIT of (float)
  | STRLIT of (string)
  | CHARLIT of (char)
  | NEW
  | DELETE
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 63 "parser.ml"
let yytransl_const = [|
  257 (* EQ *);
  258 (* NEQ *);
  259 (* GEQ *);
  260 (* LEQ *);
  261 (* GT *);
  262 (* LT *);
  263 (* AND *);
  264 (* OR *);
  265 (* NOT *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIVIDE *);
  270 (* MOD *);
  271 (* ASSIGN *);
  272 (* PLUS_ASSIGN *);
  273 (* MINUS_ASSIGN *);
  274 (* TIMES_ASSIGN *);
  275 (* DIVIDE_ASSIGN *);
  276 (* MOD_ASSIGN *);
  277 (* IF *);
  278 (* ELSE *);
  279 (* WHILE *);
  280 (* FOR *);
  281 (* BREAK *);
  282 (* CONTINUE *);
  283 (* RETURN *);
  284 (* LPAREN *);
  285 (* RPAREN *);
  286 (* LBRACKET *);
  287 (* RBRACKET *);
  288 (* LBRACE *);
  289 (* RBRACE *);
  290 (* COMMA *);
  291 (* SEMI *);
  292 (* BOOL *);
  293 (* INT *);
  294 (* FLOAT *);
  295 (* CHAR *);
  296 (* STRING *);
  297 (* STRUCT *);
  298 (* NODE *);
  299 (* EDGE *);
  300 (* GRAPH *);
  301 (* DOT *);
  302 (* TRUE *);
  303 (* FALSE *);
  308 (* NEW *);
  309 (* DELETE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* INTLIT *);
  305 (* FLOATLIT *);
  306 (* STRLIT *);
  307 (* CHARLIT *);
  310 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\007\000\007\000\003\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\008\000\008\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\012\000\012\000\013\000\
\013\000\014\000\014\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\000\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\000\000\002\000\002\000\003\000\
\003\000\005\000\007\000\009\000\005\000\000\000\001\000\000\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\001\000\001\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\001\000\003\000\004\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\008\000\000\000\
\000\000\010\000\000\000\000\000\009\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\005\000\050\000\000\000\022\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\024\000\
\053\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\039\000\000\000\000\000\000\000\052\000\000\000\000\000\
\029\000\000\000\000\000\000\000\000\000\027\000\000\000\000\000\
\028\000"

let yydgoto = "\002\000\
\003\000\004\000\014\000\015\000\016\000\021\000\028\000\032\000\
\022\000\044\000\045\000\051\000\077\000\078\000"

let yysindex = "\036\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\255\
\240\254\171\255\000\000\017\255\013\255\026\255\000\000\032\255\
\171\255\000\000\022\255\171\255\000\000\000\000\027\255\025\255\
\049\255\008\255\060\255\061\255\067\255\008\255\008\255\000\000\
\000\000\000\000\244\254\000\000\147\255\000\000\008\255\008\255\
\008\255\140\000\063\255\078\000\042\255\008\255\008\255\008\255\
\008\255\008\255\008\255\008\255\008\255\008\255\008\255\008\255\
\008\255\008\255\008\255\000\000\107\000\120\000\066\255\000\000\
\000\000\000\000\140\000\140\000\077\255\071\255\253\254\253\254\
\253\254\253\254\253\254\253\254\166\000\153\000\081\255\081\255\
\000\000\000\000\093\255\093\255\008\255\000\000\008\255\090\255\
\000\000\167\255\140\000\093\255\008\255\000\000\086\255\093\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\089\255\000\000\000\000\000\000\097\255\000\000\000\000\
\000\000\000\000\000\000\059\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\087\255\000\000\000\000\
\000\000\000\000\132\255\000\000\000\000\000\000\000\000\000\000\
\087\255\006\255\000\000\000\000\000\000\000\000\098\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\255\233\254\000\000\099\255\228\255\236\255\
\244\255\023\000\058\000\066\000\012\000\010\255\182\255\193\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\076\255\
\000\000\000\000\009\255\000\000\100\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\103\000\000\000\022\000\000\000\000\000\092\000\
\000\000\019\000\222\255\209\255\000\000\000\000"

let yytablesize = 435
let yytable = "\046\000\
\013\000\071\000\054\000\050\000\052\000\034\000\064\000\065\000\
\066\000\067\000\034\000\018\000\069\000\070\000\050\000\055\000\
\034\000\047\000\019\000\075\000\076\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\034\000\031\000\039\000\001\000\035\000\047\000\020\000\
\031\000\024\000\035\000\047\000\047\000\035\000\027\000\036\000\
\037\000\031\000\034\000\038\000\039\000\103\000\017\000\042\000\
\040\000\041\000\098\000\025\000\099\000\043\000\035\000\026\000\
\036\000\037\000\050\000\021\000\038\000\039\000\023\000\049\000\
\042\000\040\000\074\000\029\000\049\000\049\000\043\000\021\000\
\033\000\021\000\021\000\019\000\026\000\021\000\021\000\047\000\
\048\000\042\000\021\000\021\000\066\000\067\000\049\000\043\000\
\026\000\072\000\026\000\026\000\093\000\034\000\026\000\026\000\
\095\000\094\000\021\000\026\000\026\000\096\000\097\000\100\000\
\021\000\035\000\104\000\036\000\037\000\006\000\102\000\038\000\
\039\000\030\000\105\000\026\000\040\000\007\000\032\000\033\000\
\030\000\026\000\030\000\053\000\051\000\051\000\051\000\051\000\
\051\000\051\000\051\000\051\000\042\000\051\000\051\000\051\000\
\051\000\000\000\043\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\000\000\064\000\065\000\066\000\067\000\
\051\000\000\000\000\000\000\000\000\000\051\000\051\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\000\000\
\064\000\065\000\066\000\067\000\000\000\068\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\000\000\036\000\
\036\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\101\000\037\000\037\000\000\000\000\000\005\000\006\000\
\007\000\008\000\036\000\009\000\010\000\011\000\012\000\036\000\
\036\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\037\000\037\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\000\000\000\000\000\000\000\000\
\040\000\000\000\000\000\000\000\000\000\040\000\040\000\000\000\
\041\000\000\000\000\000\000\000\000\000\041\000\041\000\000\000\
\045\000\000\000\046\000\046\000\000\000\045\000\045\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\000\000\
\000\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\046\000\009\000\010\000\011\000\012\000\046\000\046\000\000\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\043\000\043\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\000\000\000\000\000\000\000\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\044\000\064\000\
\065\000\066\000\067\000\044\000\044\000\000\000\042\000\000\000\
\000\000\000\000\000\000\042\000\042\000\000\000\000\000\000\000\
\000\000\000\000\073\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\000\000\064\000\065\000\066\000\067\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\000\000\064\000\065\000\066\000\067\000\000\000\000\000\091\000\
\000\000\000\000\000\000\000\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\092\000\064\000\065\000\066\000\
\067\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\000\000\000\000\064\000\065\000\066\000\067\000\056\000\057\000\
\058\000\059\000\060\000\061\000\000\000\000\000\000\000\064\000\
\065\000\066\000\067\000"

let yycheck = "\034\000\
\000\000\049\000\015\001\038\000\039\000\029\001\010\001\011\001\
\012\001\013\001\034\001\028\001\047\000\048\000\049\000\028\001\
\009\001\008\001\035\001\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\009\001\029\001\028\001\001\000\029\001\029\001\018\000\
\035\001\029\001\034\001\034\001\035\001\021\001\025\000\023\001\
\024\001\028\000\009\001\027\001\028\001\101\000\054\001\048\001\
\032\001\033\001\093\000\034\001\095\000\054\001\021\001\032\001\
\023\001\024\001\101\000\009\001\027\001\028\001\054\001\029\001\
\048\001\032\001\033\001\054\001\034\001\035\001\054\001\021\001\
\054\001\023\001\024\001\035\001\009\001\027\001\028\001\028\001\
\028\001\048\001\032\001\033\001\012\001\013\001\028\001\054\001\
\021\001\035\001\023\001\024\001\035\001\009\001\027\001\028\001\
\034\001\029\001\048\001\032\001\033\001\091\000\092\000\022\001\
\054\001\021\001\029\001\023\001\024\001\029\001\100\000\027\001\
\028\001\035\001\104\000\048\001\032\001\029\001\029\001\029\001\
\029\001\054\001\028\000\040\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\048\001\010\001\011\001\012\001\
\013\001\255\255\054\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\010\001\011\001\012\001\013\001\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\255\255\
\010\001\011\001\012\001\013\001\255\255\035\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\255\255\010\001\
\011\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\035\001\010\001\011\001\255\255\255\255\036\001\037\001\
\038\001\039\001\029\001\041\001\042\001\043\001\044\001\034\001\
\035\001\255\255\255\255\255\255\255\255\029\001\255\255\255\255\
\255\255\255\255\034\001\035\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\255\255\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\255\255\
\029\001\255\255\007\001\008\001\255\255\034\001\035\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\039\001\
\029\001\041\001\042\001\043\001\044\001\034\001\035\001\255\255\
\255\255\255\255\255\255\029\001\255\255\255\255\255\255\255\255\
\034\001\035\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\255\255\255\255\255\255\255\255\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\029\001\010\001\
\011\001\012\001\013\001\034\001\035\001\255\255\029\001\255\255\
\255\255\255\255\255\255\034\001\035\001\255\255\255\255\255\255\
\255\255\255\255\029\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\010\001\011\001\012\001\013\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\255\255\010\001\011\001\012\001\013\001\255\255\255\255\029\001\
\255\255\255\255\255\255\255\255\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\029\001\010\001\011\001\012\001\
\013\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\011\001\012\001\013\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001"

let yynames_const = "\
  EQ\000\
  NEQ\000\
  GEQ\000\
  LEQ\000\
  GT\000\
  LT\000\
  AND\000\
  OR\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  ASSIGN\000\
  PLUS_ASSIGN\000\
  MINUS_ASSIGN\000\
  TIMES_ASSIGN\000\
  DIVIDE_ASSIGN\000\
  MOD_ASSIGN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  SEMI\000\
  BOOL\000\
  INT\000\
  FLOAT\000\
  CHAR\000\
  STRING\000\
  STRUCT\000\
  NODE\000\
  EDGE\000\
  GRAPH\000\
  DOT\000\
  TRUE\000\
  FALSE\000\
  NEW\000\
  DELETE\000\
  EOF\000\
  "

let yynames_block = "\
  INTLIT\000\
  FLOATLIT\000\
  STRLIT\000\
  CHARLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 47 "parser.mly"
            ()
# 382 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ()
# 388 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 51 "parser.mly"
              ()
# 396 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 52 "parser.mly"
              ()
# 404 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 55 "parser.mly"
                                                                      ()
# 415 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                ()
# 421 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 59 "parser.mly"
              ()
# 428 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
         ()
# 436 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                           ()
# 445 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
              ()
# 451 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 67 "parser.mly"
                   ()
# 459 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "parser.mly"
              ()
# 467 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
              ()
# 473 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
              ()
# 479 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
              ()
# 485 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
              ()
# 491 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
              ()
# 497 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
              ()
# 503 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
              ()
# 509 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
              ()
# 515 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                ()
# 521 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 86 "parser.mly"
                 ()
# 529 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
            ()
# 536 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 90 "parser.mly"
                       ()
# 543 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 91 "parser.mly"
                          ()
# 550 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
                                          ()
# 558 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "parser.mly"
                                       ()
# 567 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                                                          ()
# 577 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                                ()
# 585 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                ()
# 591 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
       ()
# 598 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                ()
# 604 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 103 "parser.mly"
            ()
# 611 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
       ()
# 618 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                       ()
# 626 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                              ( Binop(_1, Add, _3) )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                              ( Binop(_1, Sub, _3) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                              ( Binop(_1, Mul, _3) )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                              ( Binop(_1, Div, _3) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                              ( Binop(_1, Equal, _3) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                              ( Binop(_1, Neq, _3) )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                              ( Binop(_1, Less, _3) )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                              ( Binop(_1, Leq, _3) )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                              ( Binop(_1, Greater, _3) )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                              ( Binop(_1, Geq, _3) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                              ( Binop(_1, And, _3) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                              ( Binop(_1, Or, _3) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                              ( Not(_2) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                              ( Assign(_1, _3) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 124 "parser.mly"
                              ( IntLit(_1) )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
                              ( Id(_1) )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 127 "parser.mly"
                            ()
# 759 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                              ( _2 )
# 766 "parser.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
