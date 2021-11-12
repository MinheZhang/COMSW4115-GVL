# COMSW4115-GVL
This is a course project of COMS W4115. A compiler of a graph visualization language written in Ocaml.

## Scanner
### How to Compile Parser using Ocamllex
In terminal:
```
ocamllex parser.mll
```

## Parser
### How to Compile Parser using Ocamlyacc
In terminal:
```
ocamlyacc parser.mly
```
### How to test Parser using menhir
In terminal:
```
menhir --interpret --interpret-show-cst parser.mly
INT ID LPAREN RPAREN LBRACE ID LPAREN LITERAL RPAREN SEMI RBRACE EOF
```

## AST
### How to use AST Pretty Printing
In terminal:
```
ocamlbuild -use-ocamlfind gvl.native -package llvm,llvm.analysis
./gvl.native -a {test file name}.gvl
```
