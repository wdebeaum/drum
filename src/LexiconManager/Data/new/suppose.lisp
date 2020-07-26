;;;;
;;;; W::suppose
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::suppose
   (SENSES
    ((LF-PARENT ONT::SUPPOSE)
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("suppose%2:32:00"))
     (example "I suppose that going along the coast is faster")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-formal-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::SUPPOSE)
     (SEM (F::Aspect F::stage-level))
     (TEMPL experiencer-neutral-xp-templ)
     )

    )
   )
))

(define-words :pos W::v
  :tags (:base500)
  :words (
	  (W::supposed
	   (wordfeats (W::VFORM W::PASSIVE) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "It is supposed (that)...")
	     (LF-PARENT ONT::expectation)
	     (TEMPL EXPLETIVE-FORMAL-1-XP1-2-XP2-TEMPL )
	     )
	    )
	   ))
  )
