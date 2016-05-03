;;;;
;;;; W::wither
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
	  (W::wither
	    (wordfeats (W::morph (:forms (-vb) :past W::withered :ing W::withering)))
	   (SENSES
	    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("wither%2:30:00"))
	     (templ agent-affected-xp-templ)
	     (LF-PARENT ONT::shrink)
					; like ferment
	     )
	    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
	     (LF-PARENT ONT::shrink)
	     (example "the plants withered")
	     (templ affected-templ)
	     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	     )
	    )
	   )
))

