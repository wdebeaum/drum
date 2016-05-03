;;;;
;;;; W::spring
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::spring
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("spring%1:28:00"))
     (LF-PARENT ONT::spring)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::SPRING W::ONION)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

