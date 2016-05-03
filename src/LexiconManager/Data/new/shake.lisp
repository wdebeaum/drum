;;;;
;;;; W::SHAKE
;;;;

(define-words :pos W::n
 :words (
  (W::SHAKE
  (senses
	   ((LF-PARENT ONT::BEVERAGES)
	    (TEMPL count-PRED-TEMPL)
	    (SEM (F::form F::liquid))
	    )
	   )
)
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::shake
   (wordfeats (W::morph (:forms (-vb) :past W::shook :pastpart W::shaken :ing W::shaking)))
   (SENSES
    
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("shake%2:37:00"))
     (LF-PARENT ONT::evoke-excitement)
     (example "the problem shook him")
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080222 :change-date nil :comments nil)
     (LF-PARENT ONT::uncontrolled-body-motion)
     (example "his leg twitched")
     (TEMPL affected-templ)
     )
    )
   )
))

