;;;;
;;;; W::wilt
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
	  (W::wilt
	   (SENSES
	    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("wilt%2:30:00"))
	     (templ agent-affected-xp-templ)
	     (LF-PARENT ONT::deteriorate)
					; like ferment
	     )
	    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
	     (LF-PARENT ONT::deteriorate)
	     (example "the flowers wilted")
	     (w::resultative +)
	     (templ affected-templ)
	     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	     )
	    )
	   )
))

