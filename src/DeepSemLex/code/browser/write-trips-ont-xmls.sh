#!/bin/bash

output_dir=$1

. ../lisp-env.sh

set -eux

# run genericized lisp command
$LISP \
  $BATCH \
  $LOAD write-trips-ont-xmls.lisp \
  $EVAL "(run \"$output_dir\")" \
  $QUIT

