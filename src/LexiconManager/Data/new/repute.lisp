;;;;
;;;; W::repute
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::repute
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::suppose)
     (TEMPL agent-templ) ; like guess
     )
    )
   )
))

(define-words :pos W::v
  :tags (:base500)
  :words (
	  (W::reputed
	   (wordfeats (W::VFORM W::PASSIVE) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "It is reputed (that)...")
	     (LF-PARENT ONT::expectation)
	     (TEMPL EXPLETIVE-FORMAL-TEMPL )
	     )
	    )
	   ))
  )
