#!/bin/sh

sbcl --noinform --eval '(asdf:load-system "braid-algebra")' --eval '(format t "welcome to the braid algebra REPL ~%")'


