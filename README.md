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

## Docker
Invoke Docker
```
docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt
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

## Testing
`test-xxx.gvl` means the test file should pass.
`fail-xxx.gvl` means the test file should fail.

