#!/bin/sh

LISP=sbcl

## Disclaimer:
## I have not tested that all lisps take the "--load" flag.
## This code might spontaneously combust your whole everything.

$LISP --load "dump-db.lisp"
