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

## Compile Command:

### For Docker
Build.
```
opam config exec -- ocamlbuild -use-ocamlfind gvl.native
```
Build clean.
```
opam config exec -- ocamlbuild -clean
```
