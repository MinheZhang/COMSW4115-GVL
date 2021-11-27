{
open Parser
exception ScannerError of string
}

let whitespace = [' ' '\t' '\r' '\n']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ascii_character = [' '-'!' '#'-'&' '('-'[' ']'-'~']  (* matches ascii printable char except quotes & backslash *)

rule tokenize = parse
  whitespace { tokenize lexbuf }
(* SEPARATORS *)
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '{' { LBRACE }
| '}' { RBRACE }
| ',' { COMMA }
| ';' { SEMI }
(* COMMENTS *)
| "//" { s_comment lexbuf }
| "/*" { m_comment lexbuf }
(* OPERATORS *)
| "==" { EQ }
| "!=" { NEQ }
| ">=" { GEQ }
| "<=" { LEQ }
| "+=" { PLUS_ASSIGN }
| "-=" { MINUS_ASSIGN }
| "*=" { TIMES_ASSIGN }
| "/=" { DIVIDE_ASSIGN }
| "%=" { MOD_ASSIGN }
| "&&" { AND }
| "||" { OR }
| '=' { ASSIGN }
| '>' { GT }
| '<' { LT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
(*| '&' { BIT_AND }
| '|' { BIT_OR }*)
| '!' { NOT }
| '.' { DOT }
| ':' { COLON }
| "++" { PLUSPLUS }
| "--" { MINUSMINUS }
(* CONTROL FLOW KEYWORDS *)
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "break" { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
(* TYPES *)
| "bool" { BOOL }
| "int" { INT }
| "float" { FLOAT }
| "char" { CHAR }
| "string" {STRING}
| "struct" { STRUCT }
| "node" { NODE }
| "edge" { EDGE }
| "graph" { GRAPH }
(* CONSTANTS *)
| "true" { BOOLLIT(true) }
| "false" { BOOLLIT(false) }
| digit+ as lxm { INTLIT(int_of_string lxm) }
| (digit+) '.' (digit+) as lxm { FLOATLIT(lxm) }
| '"' { STRLIT(string_const "" lexbuf) }
(* CHAR CONSTANTS *)
| ''' ((ascii_character | '"') as lxm) ''' { CHARLIT(lxm) }
| ''' "\\\\" ''' { CHARLIT('\\') }
| ''' "\\n" ''' { CHARLIT('\n') }
| ''' "\\t" ''' { CHARLIT('\t') }
| ''' "\\r" ''' { CHARLIT('\r') }
(* IDENTIFIERS *)
| letter (letter | digit | '_')* as id { ID(id) }
(* EOF *)
| eof { EOF }
(* UNDEFINED *)
| _ { raise (ScannerError "illegal character") }

and s_comment = parse
  '\n' { tokenize lexbuf }
| eof { EOF }
| '_' { s_comment lexbuf }

and m_comment = parse
  "*/" { tokenize lexbuf }
| eof { raise (ScannerError "unterminated comment") }
| '_' { m_comment lexbuf }

and string_const result_str = parse
  '"' { result_str }
| "\\\\" { string_const (result_str ^ "\\") lexbuf }
| "\\n" { string_const (result_str ^ "\n") lexbuf }
| "\\t" { string_const (result_str ^ "\t") lexbuf }
| "\\r" { string_const (result_str ^ "\r") lexbuf }
| (ascii_character | ''') as c { string_const (result_str ^ (String.make 1 c)) lexbuf}
| eof { raise (ScannerError "unterminated comment") }
| _ { raise (ScannerError "illegal character") }