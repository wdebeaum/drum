;;;;
;;;; W::SPICE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SPICE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("spice%1:27:00" "spice%1:13:00"))
     (LF-PARENT ONT::SUBSTANCE)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::spice
  (senses
	   ((LF-PARENT ONT::SPICES-HERBS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

