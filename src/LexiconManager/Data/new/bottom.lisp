;;;;
;;;; W::BOTTOM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BOTTOM
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::BOTTOM-location)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::BOTTOM W::SIRLOIN W::BUTT)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::BOTTOM W::ROUND)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::BOTTOM W::SIRLOIN)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
;   )
  (W::BOTTOM
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("bottom%3:00:00"))
     (LF-PARENT ONT::BOTTOM-LOCATION-VAL)
     )
    )
   )
))

