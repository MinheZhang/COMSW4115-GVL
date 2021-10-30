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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
