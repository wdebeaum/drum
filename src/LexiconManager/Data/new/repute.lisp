;;;;
;;;; W::repute
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::repute
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::suppose)
     (TEMPL experiencer-templ) ; like guess
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
	     (TEMPL EXPLETIVE-FORMAL-1-XP1-2-XP2-TEMPL )
	     )
	    )
	   ))
  )
