# GVL
This is a course project of [COMS W4115 Fall 2021](http://www.cs.columbia.edu/~sedwards/classes/2021/4115-fall/index.html) 
instructed by Prof. [Stephen A. Edwards](http://www.cs.columbia.edu/~sedwards/index.html). 
GVL is an abbreviation for Graph Visualization Language.
The GVL designed this language and implemented the corresponding compiler in Ocaml.

## Team member
- Yaxin Chen (yc3995): Language Guru
- Minhe Zhang (mz2864): System Architect
- Jiawen Yan (jy3088): Manager
- Aoxue We (w3389): Tester

## Reference
Thanks for all the people who have been sharing knowledge for free. This project will not progress
smoothly with your generosity.

This project is mostly adapted from [MicroC](http://www.cs.columbia.edu/~sedwards/classes/2021/4115-fall/index.html)
which is designed and implemented by 
Prof. [Stephen A. Edwards](http://www.cs.columbia.edu/~sedwards/index.html).
There are other sources we have consulted not only official documents but also tech blogs:
- https://github.com/glfw/glfw/tree/master/examples
- https://discourse.glfw.org/t/a-very-basic-issue-with-draw-line/804/2
- https://newbedev.com/drawing-circle-with-opengl
- https://shnoh171.github.io/gpu%20and%20gpu%20programming/2019/08/26/installing-glfw-on-ubuntu.html
- https://stackoverflow.com/questions/9148890/how-to-make-clang-compile-to-llvm-ir
- https://llvm.moe/ocaml/Llvm.html
- https://llvm.org/docs/GettingStarted.html
- https://ocaml.org/manual/index.html#sec6
- https://llvm.org/docs/ProgrammersManual.html
- http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/JavaLite.pdf
- http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/Graphene.pdf
- http://www.cs.columbia.edu/~sedwards/classes/2021/4115-spring/reports/Konig.pdf
- https://github.com/matteosandrin/konig

## Installation

### Environment Setup
Setup basic tool.
```
sudo apt install install ocaml llvm-10.0 llvm-10.0-dev llvm-runtime m4 opam clang
opam init
opam install llvm.10.0.0
```
Install graphic programmming tool.
```
sudo apt update
sudo apt install xorg-dev
sudo apt install libglfw3
sudo apt install libglfw3-dev
sudo apt install libglu1-mesa-dev
git clone https://github.com/Dav1dde/glad.git
cd glad
cmake./
make
sudo cp -a include /usr/local/
sudo cp libglad.a /usr/lib/x86_64-linux-gnu/
```

### Compiler Compilation
Download GVL source code from https://github.com/MinheZhang/COMSW4115-GVL.git
```
git clone https://github.com/MinheZhang/COMSW4115-GVL.git
```
The compiler can be built with `make`. This will also run all the tests.
```
cd COMSW4115-GVL/src
make clean
make
```

### Compile and Run GVL source code.
```
./run.sh <.gvl file>
```

## Tasks List
### Iteration 1
- ~~Condition Statement~~
- ~~Function Call (Argument)~~
- ~~Loop Statement~~

### Iteration 2
- ~~Node~~
- ~~Edge~~

### Iteration 3
- ~~Visualization~~
  
### Iteration 4
- Node & Edge Extension
- Syntax Sugar

---

## Useful Commands for Developer
The information below are only for developers who want to extend GVL compiler.
If you are not interested in writing comiler, just ignore them.

### Compile Scanner
```
ocamllex scanner.mll
```

### Compile Parser
```
ocamlyacc parser.mly
```

### Docker
List all images.
```
docker images -a
```
Remove images.
```
docker rmi <image1> <image2> ...
```
Build image from `Dockerfile`.
```
docker build -f <path to>/Dockerfile -t <repo-name> .
```
Invoke Docker
```
docker run --rm -it -v `pwd`:/home/gvl -w=/home/gvl <repo-name>
```
- `--rm`: Automatically remove the container when it exits
- `-it`: Interactive and pseudo terminal
- `-v`: Mounts the current working directory into the container
- `-w`: Let the command being executed inside the current working directory
- `<repo-name>`: Name of repository

### Compile GVL Compiler in Docker
Build.
```
opam config exec -- ocamlbuild -use-ocamlfind gvl.native
```
Build clean.
```
opam config exec -- ocamlbuild -clean
```

### Generate IR
From c.
```
clang -S -emit-llvm foo.c
```

## Testing
`test-xxx.gvl` means the test file should pass.

`fail-xxx.gvl` means the test file should fail.
