#!/bin/bash

# Usage: ./gloss-output-to-dsl ":pos 'v" <facilitator.log >output.lisp
# (or ":pos 'adj". Other POSes are currently unimplemented.)

export TRIPS_BASE=../../../..
. ../lisp-env.sh
mk2env $CONFIGDIR/ruby/defs.mk
mk2env $CONFIGDIR/perl/defs.mk

# Note: can't use uniq to remove duplicate lines, because their length may
# exceed LINE_MAX, and uniq on Mac OS X truncates such lines.
$PERL -n -e 'print "$&\n" while (/\(DEFINE-CONCEPT .*\)(?=\)$)/g);' \
| sort \
| $PERL -n -e 'print unless ($_ eq $prev); $prev = $_;' \
| $LISP \
  $BATCH \
  $LOAD gloss-output-to-dsl.lisp \
  $EVAL "(run $@)" \
  $QUIT \
| $RUBY gloss-output-to-dsl.rb

# TODO
# ? include lattice that gloss used as input (have to get it from somewhere other than facilitator.log)
# ? get rid of excessive ONT:: prefixes in LF terms
