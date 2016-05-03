;;;;
;;;; W::establish
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
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
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like reveal,prove
     )
    ((LF-PARENT ONT::establish)
     (example "establish an organization on principle")
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     (TEMPL agent-affected-create-manner-optional-templ)
     )
    ((LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
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
	     (TEMPL EXPLETIVE-FORMAL-TEMPL (xp1 (% W::NP (W::lex W::it))) )
	     )
	    )
	   ))
  )

