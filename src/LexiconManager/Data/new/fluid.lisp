;;;;
;;;; W::fluid
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::fluid
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("fluid%1:27:00" "fluid%1:27:02"))
     (LF-PARENT ONT::liquid-substance)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::FLUID W::REPLACEMENT)
  (senses
	   ((LF-PARENT ONT::BEVERAGES)
	    (TEMPL MASS-PRED-TEMPL)
	    (SEM (F::form F::liquid))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::FLUID W::REPLACEMENT)
  (senses
	   ((LF-PARENT ONT::BEVERAGES)
	    (TEMPL count-PRED-TEMPL)
	    (SEM (F::form F::liquid))
	    )
	   )
)
))

