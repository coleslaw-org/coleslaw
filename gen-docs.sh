#!/bin/sh
sbcl --eval "(ql:quickload '(coleslaw sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :coleslaw \"docs/coleslaw.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
