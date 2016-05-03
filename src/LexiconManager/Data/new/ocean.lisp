;;;;
;;;; W::OCEAN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::OCEAN
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("ocean%1:17:00"))
     (LF-PARENT ONT::geo-formation)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::OCEAN W::PERCH)
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::OCEAN W::POUT)
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

