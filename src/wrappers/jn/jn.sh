#!/bin/bash -x

# JABS Naked script

ME=$(realpath $(dirname $(readlink -f $0)))/../../../

test -z $LISP && LISP="sbcl"

case $LISP in
    "sbcl")
	EXEC_STRING="$LISP --noinform --no-userinit --no-sysinit --non-interactive --load ${ME}/src/jabs-loader.lisp -Ddebug=t $@"
	;;
    *)
	echo "Lisp support for $LISP is not implemented"
	exit 1
	;;
esac

$EXEC_STRING
