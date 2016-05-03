;;;;
;;;; W::sugar
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
(W::sugar
   (SENSES
    ((meta-data :origin caet :entry-date 20111220)
     (LF-PARENT ONT::sugar)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
)
))

(define-words :pos W::n
 :words (
  ((W::SUGAR w::APPLE)
  (senses
	   ((LF-PARENT ONT::FRUIT)	    
	    (TEMPL COUNT-PRED-TEMPL)
	    
	    )
	   )
)
))

