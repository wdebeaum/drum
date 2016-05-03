;;;;
;;;; W::acid
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::acid
   (SENSES
    ((meta-data :origin medadvisor :entry-date 20060803 :change-date nil :comments nil :wn ("acid%1:27:00"))
     (LF-PARENT ONT::liquid-substance)
     (TEMPL BARE-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::ACID W::WASH)
  (senses
	   ((LF-PARENT ONT::food-prep-PROCESS)
	    (syntax (W::morph (:forms (-none))))
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

