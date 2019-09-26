;;;;
;;;; W::establish
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::establish
     (wordfeats (W::morph (:forms (-vb) :nom w::establishment)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (example "establish the likelihood of the story")
     (TEMPL agent-neutral-xp-templ) ; like reveal,prove
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::show)
     (example "establish that...")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like reveal,prove
     )
    ((LF-PARENT ONT::establish)
     (example "establish an organization")
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     ;(TEMPL AGENT-AFFECTEDR-MANNER-2-XP-3-XP2-OPTIONAL-TEMPL)
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
     )
    ((LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    )
   )
))


(define-words :pos W::v
  :tags (:base500)
  :words (
	  (W::established
	   (wordfeats (W::VFORM W::PASSIVE) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "It has been established that...")
	     (LF-PARENT ONT::CORRELATION)
	     (TEMPL EXPLETIVE-FORMAL-1-XP1-2-XP2-TEMPL)
	     )
	    )
	   ))
  )

