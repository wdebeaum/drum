#!/bin/bash

export TRIPS_BASE=../../../../
. ../lisp-env.sh
$LISP \
  $BATCH \
  $LOAD dsl-to-xml.lisp \
  $EVAL '(run)' \
  $QUIT \
| grep -v '^;'

