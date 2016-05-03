#!/bin/bash

# usage:
# get-entities-from-tt.sh facilitator.log brat.tsv

flog=$1
out=$2
start=$3

DEFAULT_PSTART=3

PSTART=${start:-$DEFAULT_PSTART}

export my_dir=`dirname $0`
export TRIPS_BASE=${TRIPS_BASE:-$my_dir/../../..}

(
$TRIPS_BASE/bin/extract-messages -sender TextTagger <$flog
) \
| sbcl --disable-debugger \
  --load $my_dir/get-entities-from-tt.lisp \
  --eval "(with-open-file (out \"$out\" :direction :output :if-exists :supersede) (run *standard-input* out :first-passage-num $PSTART))" \
  --eval "(quit)"

# for CCL, replace the "| sbcl" line above with this
#| lisp -b -Q \

