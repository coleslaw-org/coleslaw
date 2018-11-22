#!/bin/sh

LISP=sbcl

### DON'T EDIT BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING. ###
DUMP="dump-db.lisp"
if [ "$LISP"="cmucl" ] || [ "$LISP"="lispworks" ] || [ "$LISP"="gcl" ] || [ "$LISP"="abcl" ];
then $LISP -load $DUMP
else
    if [ "$LISP"="clisp" ];
    then $LISP -i $DUMP
    else
	if [ "$LISP"="allegro" ];
	then $LISP -l $DUMP
	else $LISP --load $DUMP #SBCL CCL ECL
	fi
    fi
fi
