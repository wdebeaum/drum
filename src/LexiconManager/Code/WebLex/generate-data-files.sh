#!/bin/bash

trips_src_dir=$1
lexicon_data_dir=$2

echo "Using the TRIPS source directory $trips_src_dir"
echo "to update the lexicon data directory $lexicon_data_dir ..."

echo "  removing old data files ..."
rm $lexicon_data_dir/{ONT,W}\:\:*.xml
rm -f $lexicon_data_dir/TRIPS-ontology.xml
rm -f $lexicon_data_dir/trips-ont.omn

echo "  generating new data files ..."
# CMUCL
#cat - <<EOP | lisp -quiet -batch
#:cd "$trips_src_dir/Parser"
#(load "parser")
#:cd "$trips_src_dir/LexiconManager/Code/WebLex/"
# SBCL
cat - <<EOP | lisp --noinform --noprint
(setf *default-pathname-defaults* #P"$trips_src_dir/Parser/")
(load "parser")
(setf *default-pathname-defaults* #P"$trips_src_dir/LexiconManager/Code/WebLex/")

(load "lisp2xml")
(load "onttypes2xml")
(load "words2xml")
(format t "(write-all-ont-type-xmls \"$lexicon_data_dir\") ...~%")
(write-all-ont-type-xmls "$lexicon_data_dir")
(format t "(write-all-word-xmls \"$lexicon_data_dir\") ...~%")
(write-all-word-xmls "$lexicon_data_dir")
(format t "(write-single-ontology-xml \"$lexicon_data_dir\") ...~%")
(write-single-ontology-xml "$lexicon_data_dir")
(format t "; done with lisp processing.~%")
EOP
# lisp doesn't print a newline when it quits :(
echo

lisp --load make-trips-ont-owl.lisp --quit
mv trips-ont.omn $lexicon_data_dir/

# Filter the newly created files to change the "modified" field from UNIX 
# timestamp to human-readable time
echo "  making 'modified' attribute a human-readable time ..."
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/{ONT,W}\:\:*.xml

echo "done."

