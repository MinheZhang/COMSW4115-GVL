%{ open Ast %}
/* operators: logical */
%token EQ NEQ GEQ LEQ GT LT AND OR NOT
/* operators: arithmetic */
%token PLUS MINUS TIMES DIVIDE MOD
/* operators: assignment */
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN
/* operators: graph */
%token PLUSPLUS MINUSMINUS
/* controal flow keywords*/
%token IF ELSE WHILE FOR BREAK CONTINUE RETURN
/* separators */
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SEMI
/* types */
%token BOOL INT FLOAT CHAR STRING STRUCT NODE EDGE GRAPH LIST LISTITER
/* node/edge extension */
%token COLON
/* reference */
%token DOT
/* constants */
%token <bool> BOOLLIT
%token <int> INTLIT
%token <string> FLOATLIT
%token <string> STRLIT
%token <char> CHARLIT
/* identifiers */
%token <string> ID
/* end of file */
%token EOF

%nonassoc NOELSE
%nonassoc PLUSPLUS MINUSMINUS
%left ELSE
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN
%left OR
%left AND
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%start program
// %type <Ast.expr> expr
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { ([], []) }
| decls vdecl { (($2 :: fst $1), snd $1) }
| decls fdecl { (fst $1, ($2 :: snd $1)) }
// TODO
// struct declaration.
//| decls sdecl {}
// array declaration.
//| decls adecl_assign {}

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
   fname = $2;
   formals = List.rev $4;
   body = List.rev $7 } }

formals_opt: 
  /* nothing */ { [] }
| formal_list { $1 }

formal_list:
  typ ID { [($1, $2)] }
| formal_list COMMA typ ID { ($3, $4) :: $1 }

// TODO in struct
// vdecl_list:
// /* nothing */ { [] }
// | vdecl_list vdecl { $2 :: $1 }

vdecl: 
  typ ID SEMI { ($1, $2) }
| typ ID ASSIGN expr SEMI { ($1, $2) }

// TODO
/*
// Array declaration and assignment.
adecl_assign:
  typ ID ASSIGN array_lit SEMI {}

// Structure declaration and assignment.
sdecl:
  STRUCT ID LBRACE vdecl_list RBRACE SEMI {}
  // Inheritance of node and edge using struct.
| STRUCT ID COLON NODE LBRACE vdecl_list RBRACE SEMI {}
| STRUCT ID COLON EDGE LBRACE vdecl_list RBRACE SEMI {}
*/

typ:
  BOOL        { Bool }
| INT         { Int }
| FLOAT       { Float }
| CHAR        { Char }
| STRING      { String }
| STRUCT ID   { StructID }
| NODE        { Node }
| EDGE        { Edge }
| GRAPH       { Graph }
| LIST        { GvlList }
<<<<<<< HEAD
=======
| LISTITER    { GvlListIterator }
>>>>>>> main
//| typ LBRACKET expr RBRACKET {}  // TODO


/* statements */

stmt_list:
  /* nothing */  { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI                               { Expr ($1) }
| vdecl                                   { Vdecl($1) }
| LBRACE stmt_list RBRACE                 { Block(List.rev $2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt           { While($3, $5) }
// | BREAK SEMI                              { Break }
// | CONTINUE SEMI                           { Continue }
| RETURN expr SEMI                        { Return($2) }


expr_opt:
  /* nothing */ { Noexpr }
| expr { $1 }

args_opt:
  /* nothing */ { [] }
| args_list { List.rev $1 }

args_list:
  expr { [$1] }
| args_list COMMA expr { $3 :: $1 }

expr:
  expr PLUS   expr            { Binop($1, Add, $3) }
| expr MINUS  expr            { Binop($1, Sub, $3) }
| expr TIMES  expr            { Binop($1, Mul, $3) }
| expr DIVIDE expr            { Binop($1, Div, $3) }
| expr MOD expr               { Binop($1, Mod, $3) }
//| expr PLUSPLUS expr          { }  // TODO
//| expr MINUSMINUS expr        { }
| expr EQ  expr               { Binop($1, Equal, $3) }
| expr NEQ expr               { Binop($1, Neq, $3) }
| expr LT expr                { Binop($1, Less, $3) }
| expr LEQ expr               { Binop($1, Leq, $3) }
| expr GT expr                { Binop($1, Greater, $3) }
| expr GEQ expr               { Binop($1, Geq, $3) }
| expr AND expr               { Binop($1, And, $3) }
| expr OR expr                { Binop($1, Or, $3) }
| MINUS expr %prec NOT        { Unop(Neg, $2) }                 
| NOT expr                    { Unop(Not, $2) }
| id ASSIGN expr              { Assign($1, $3) }
/*| id PLUS_ASSIGN expr         {}
| id MINUS_ASSIGN expr        {}
| id DIVIDE_ASSIGN expr       {}
| id TIMES_ASSIGN expr        {}
| id MOD_ASSIGN expr          {}*/ // TODO
| id                          { Id($1) }
| INTLIT                      { IntLit($1) }
| BOOLLIT                     { BoolLit($1) }
| FLOATLIT                    { FloatLit($1) }
| CHARLIT                     { CharLit($1) }
| STRLIT                      { StrLit($1) }
  /* function call */
| ID LPAREN args_opt RPAREN   { Call($1, $3) }
  // Primary expression
| LPAREN expr RPAREN          { $2 }

id:
  ID                          { $1 }
//| id DOT ID                   {} // TODO
  // Array 
//| id LBRACKET expr RBRACKET   {} // TODO

// TODO
/*
array_lit:
  // {1, 2, 3}
  LBRACE args_list RBRACE        {}
  // {{1, 2}, {3, 4}}
| LBRACE array_lit_list RBRACE   {}

array_lit_list:
  array_lit                      {}
| array_lit_list COMMA array_lit {}
*/
