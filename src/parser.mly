%{ open Ast %}
/* operators: logical */
%token EQ NEQ GEQ LEQ GT LT AND OR NOT
/* operators: arithmetic */
%token PLUS MINUS TIMES DIVIDE MOD
/* operators: assignment */
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN
/* controal flow keywords*/
%token IF ELSE WHILE FOR BREAK CONTINUE RETURN
/* separators */
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SEMI
/* types */
%token BOOL INT FLOAT CHAR STRING STRUCT NODE EDGE GRAPH
/* reference */
%token DOT
/* constants */
%token TRUE FALSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRLIT
%token <char> CHARLIT
/* memory */
%token NEW DELETE
/* identifiers */
%token <string> ID
/* end of file */
%token EOF

%nonassoc NOELSE
%left SEMI
%left IF THEN ELSE
%left ASSIGN
%left OR
%left AND
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%start program
// %type <Ast.expr> expr
%type <Ast.program> program

%%

program:
  decls EOF {}

decls:
  /* nothing */ {}
| decls vdecl {}
| decls fdecl {}
| decls sdecl {}
| decls adecl_assign {}

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE {}

formals_opt: 
  /* nothing */ {}
| formal_list {}

formal_list:
  typ ID {}
| formal_list COMMA typ ID {}

vdecl_list:
/* nothing */ {}
| vdecl_list vdecl {}

vdecl: 
  typ ID SEMI {}

adecl_assign:
  typ ID ASSIGN array_lit {}

sdecl:
  STRUCT ID LBRACE vdecl_list RBRACE SEMI {}

typ:
  BOOL        {}
| INT         {}
| FLOAT       {}
| CHAR        {}
| STRUCT ID   {}
| NODE        {}
| EDGE        {}
| GRAPH       {}
| typ LBRACKET expr RBRACKET {}

/* statements */

stmt_list:
  /* nothing */ {}
| stmt_list stmt {}

stmt:
  expr SEMI                               { Expr ($1) }
| LBRACE stmt_list RBRACE                 { Expr($2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3,$5) }
| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt           { While($3, $5) }
| BREAK SEMI                              { Break }
| CONTINUE SEMI                           { Continue }
| RETURN expr SEMI                        { Return($2) }

expr_opt:
  /* nothing */ {}
| expr {}

args_opt:
  /* nothing */ {}
| args_list {}

args_list:
  expr {}
| args_list COMMA expr {}

expr:
  expr PLUS   expr            { Binop($1, Add, $3) }
| expr MINUS  expr            { Binop($1, Sub, $3) }
| expr TIMES  expr            { Binop($1, Mul, $3) }
| expr DIVIDE expr            { Binop($1, Div, $3) }
| expr EQ  expr               { Binop($1, Equal, $3) }
| expr NEQ expr               { Binop($1, Neq, $3) }
| expr LT expr                { Binop($1, Less, $3) }
| expr LEQ expr               { Binop($1, Leq, $3) }
| expr GT expr                { Binop($1, Greater, $3) }
| expr GEQ expr               { Binop($1, Geq, $3) }
| expr AND expr               { Binop($1, And, $3) }
| expr OR expr                { Binop($1, Or, $3) }
| NOT expr                    { Not($2) }
| id ASSIGN expr              { Assign($1, $3) }
| id                          { Id($1) }
| INTLIT                      { IntLit($1) }
/* function call */
| ID LPAREN args_opt RPAREN {}
| LPAREN expr RPAREN          { Expr($2) }

id:
  ID                          {}
| id DOT ID                   {}
| id LBRACKET expr RBRACKET   {}

array_lit:
  LBRACE args_list RBRACE        {}
| LBRACE array_lit_list RBRACE   {}

array_lit_list:
  array_lit                      {}
| array_lit_list COMMA array_lit {}
