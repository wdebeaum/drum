;;;;
;;;; W::JUICE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::JUICE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("juice%1:13:00"))
     (LF-PARENT ONT::FOOD)
     (SEM (F::form F::liquid))
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (W::JUICE
  (senses
	   ((LF-PARENT ONT::JUICE)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (W::JUICE
  (senses
	   ((LF-PARENT ONT::JUICE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

