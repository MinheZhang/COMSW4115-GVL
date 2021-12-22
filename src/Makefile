# "make test" Compiles everything and runs the regression tests
GCC=gcc

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : gvl.native printbig.o

# "make gvl.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

gvl.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind gvl.native \
	-package llvm -package llvm.analysis -package llvm.bitreader
	$(GCC) -c lib/graph.c lib/list.c lib/graph_visualization.c -lGL -lGLU -lglfw -lX11 -lXxf86vm -lXrandr -lpthread -lXi -lglad -ldl -lm
	clang -emit-llvm -c lib/graph.c -o graph
	$(GCC) -c lib/list.c
	clang -emit-llvm -c lib/list.c -o list

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff *.o

# Testing the "printbig" example

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# Building the tarball

TESTS = \
  func1

FAILS = \
  func1

TESTFILES = $(TESTS:%=test-%.gvl) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.gvl) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags gvl.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

gvl.tar.gz : $(TARFILES)
	cd .. && tar czf gvl/gvl.tar.gz \
		$(TARFILES:%=gvl/%)
