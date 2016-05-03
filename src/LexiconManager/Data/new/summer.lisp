;;;;
;;;; W::summer
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::summer
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("summer%1:28:00"))
     (LF-PARENT ONT::summer)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::SUMMER W::SQUASH)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

