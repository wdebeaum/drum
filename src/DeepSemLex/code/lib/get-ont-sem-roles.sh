#!/bin/sh

# get the set of semantic roles used in TRIPS, specifically in the ontology and
# templates

( cat $TRIPS_BASE/src/LexiconManager/Data/templates/*.lisp \
  | perl -e 'undef $/; $_=<>; s/#\|.*?\|#//sg; print;' \
  | perl -n -e 's/;.*//; print lc($1) . "\n" while (/ont::([^\s\(\)]+)/gi);' ;
  cat $TRIPS_BASE/src/OntologyManager/Data/LFdata/*.lisp \
  | perl -e 'undef $/; $_=<>; s/#\|.*?\|#//sg; print;' \
  | perl -n -e 's/;.*//; print lc($1) . "\n" while (/:(?:optional|required|essential|implements) (?:ont::)?([^:\s\(\)]+)(?!:)/gi);'
) |sort |uniq |perl -p -e 's/\n/ /;'
echo
