#!/bin/bash

# USAGE: ./graffle-to-terms.sh <foo.graffle >foo.lisp

# SBCL:
#./graffle-to-dot.rb | sbcl --noinform --load dot-to-terms.lisp --eval '(format t "~s~%" (convert-dot-to-terms (read-dot-graph *standard-input*)))' |grep -v '^\(;\|*\)'

# CMUCL:
#./graffle-to-dot.rb | lisp -batch -quiet -load dot-to-terms.lisp -eval '(format t "~s~%" (convert-dot-to-terms (read-dot-graph *standard-input*)))' |grep -v 'CL-USER>' |grep -v 'Top Level Command Interpreter'

# CCL:
./graffle-to-dot.rb | lisp --quiet --load dot-to-terms.lisp --eval '(format t "~s~%" (convert-dot-to-terms (read-dot-graph *standard-input*)))' |grep -v '^\(;\|?\|Welcome\)'
