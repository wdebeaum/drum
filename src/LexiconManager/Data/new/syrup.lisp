;;;;
;;;; W::syrup
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::syrup
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090402 :change-date nil :comments nil)
     (LF-PARENT ONT::liquid-substance)
     (example "cough syrup")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::syrup
  (senses
	   ((LF-PARENT ONT::SWEETS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

