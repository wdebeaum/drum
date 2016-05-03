;;;;
;;;; W::METER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::METER
   (abbrev w::m)
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("meter%1:23:00"))
     (LF-PARENT ONT::length-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	 (w::meter
	  (senses ((lf-parent ont::device)
		   (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s)
		   )))
))

