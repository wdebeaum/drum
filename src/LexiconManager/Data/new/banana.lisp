;;;;
;;;; W::BANANA
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BANANA
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("banana%1:13:00"))
     (LF-PARENT ONT::fruit)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::BANANA W::PEPPER)
  (senses
	   ((LF-PARENT ONT::CONDIMENTS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::BANANA W::BREAD)
  (senses
	   ((LF-PARENT ONT::BREAD)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

