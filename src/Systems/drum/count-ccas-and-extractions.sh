#!/bin/sh

flog=$1

echo "utterances:"
$TRIPS_BASE/bin/extract-messages -sender TextTagger :content '\(utterance' <$flog |wc -l
echo "speech acts:"
$TRIPS_BASE/bin/extract-messages -sender PARSER :content '\(NEW-SPEECH-ACT-HYPS' <$flog |wc -l
echo "CCAs: "
$TRIPS_BASE/bin/extract-messages -sender PARSER :content '\(NEW-SPEECH-ACT-HYPS \(\(COMPOUND-COMMUNICATION-ACT' <$flog |wc -l
echo "UTTs:"
$TRIPS_BASE/bin/extract-messages -sender PARSER :content '\(NEW-SPEECH-ACT-HYPS' <$flog \
| sbcl --noinform --disable-debugger --eval '(progn
  (dolist (p (list :w :ont :f :hgnc :up :pc :chebi :bto :co :go :mi :xfam :mesh))
    (make-package p))
  (loop
    with num-utts = 0
    for msg = (read *standard-input* nil)
    while msg
    for content = (second (member :content msg))
    for first-sa = (first (second content))
    do
      (ecase (first first-sa)
	(utt (incf num-utts))
	(compound-communication-act
	  (incf num-utts (length (cdr first-sa))))
	)
    finally (format t "~s~%" num-utts)
    )
  (sb-ext:quit)
  )'
echo "extraction-results: "
$TRIPS_BASE/bin/extract-messages -sender IM :content '\(EXTRACTION-RESULT' <$flog |wc -l
