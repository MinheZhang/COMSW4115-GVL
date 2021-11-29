# COMSW4115-GVL
This is a course project of COMS W4115. A compiler of a graph visualization language written in Ocaml.

## TODO List
### Iteration 1
- ~~Condition Statement~~
- ~~Function Call (Argument)~~
- ~~Loop Statement~~

### Iteration 2
- Node
- Edge
- 1-D Array

### Iteration 3
- Visualization
  
### Iteration 4
- Node & Edge Extension

---
## Useful Command

### Compile Scanner
```
ocamllex scanner.mll
```

### Compile Parser
```
ocamlyacc parser.mly
```

### Docker
Invoke Docker
```
docker run --rm -it -v `pwd`:/home/gvl -w=/home/gvl columbiasedwards/plt
```
- `--rm`: Automatically remove the container when it exits
- `-it`: Interactive and pseudo terminal
- `-v`: Mounts the current working directory into the container
- `-w`: Let the command being executed inside the current working directory
- `columbiasedwards/plt`: Name of repository

### Compile GVL Compiler in Docker
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
