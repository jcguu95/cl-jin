#!/bin/sh

TARGET="/root/curfew.lisp"
RUN="run.lisp"

sleep 10
sbcl --eval "
  (progn
    (require 'asdf)
    (push #P\"$TARGET/\" asdf:*central-registry*)
    (print asdf:*central-registry*)
    (asdf:load-system :jin.curfew)
    (load \"$TARGET/$RUN\")
)
"

exit
