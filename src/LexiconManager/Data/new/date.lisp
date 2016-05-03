;;;;
;;;; W::DATE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DATE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("date%1:28:04"))
     (LF-PARENT ONT::time-point)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (W::DATE
  (senses
	   ((LF-PARENT ONT::FRUIT)	    
	    (TEMPL COUNT-PRED-TEMPL)
	    (preference .97)
	    )
	   )
)
))

