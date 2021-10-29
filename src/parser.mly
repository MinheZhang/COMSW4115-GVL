%{ open Ast %}

(* operators: logical *)
%token EQ NEQ GEQ LEQ GT LT AND OR NOT
(* operators: arithmetic *)
%token PLUS MINUS TIMES DIVIDE MOD
(* operators: assignment *)
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN
(* controal flow keywords*)
%token IF ELSE WHILE FOR BREAK CONTINUE RETURN
(* separators *)
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA SEMMI
(* types *)
%token BOOL INT FLOAT CHAR STRUCT NODE EDGE GRAPH
(* reference *)
%token DOT
(* constants *)
%token TRUE FALSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRLIT
%token <char> CHARLIT
(* memory *)
%token NEW DELETE
(* identifiers *)
%token <string> ID
(* end of file*)
%token EOF

%left SEMMI
%left IF THEN ELSE
%left ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%


expr:
  expr PLUS   expr            { Binop($1, Add, $3) }
| expr MINUS  expr            { Binop($1, Sub, $3) }
| expr TIMES  expr            { Binop($1, Mul, $3) }
| expr DIVIDE expr            { Binop($1, Div, $3) }
| ID ASSIGN expr              { Assign($1, $3) }
| IF LPAREN expr RPAREN block ELSE block
                              { If($3, $5, $7) }
| INTLIT                      { IntLit($1) }
| ID                          { Id($1) }
| expr SEMMI expr             { Semmi($1, $3) }
