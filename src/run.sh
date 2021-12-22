# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the gvl compiler.
GVL="./gvl.native"

error=0
keep=0

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.gvl//'`
    reffile=`echo $1 | sed 's/.gvl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    Run "$GVL" "$1" ">" "${basename}.ll" &&
    Run "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" "list.o" "graph.o" "graph_visualization.o" " -lGL -lGLU -lglfw -lX11 -lXxf86vm -lXrandr -lpthread -lXi -lglad -ldl -lm" &&
    Run "./${basename}.exe" > "${basename}.out"

    # Report the status and clean up the generated files

	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	globalerror=$error
}

Check $1
