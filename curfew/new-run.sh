#!/bin/sh

TARGET="/root/curfew.lisp/"

sbcl --eval "
  (progn
    (require 'asdf)
    (push #P\"$TARGET\")
    (asdf:load-system :jin.curfew)
    (loop (ignore-errors (jin.curfew:main)) (sleep 4)))
"

exit
