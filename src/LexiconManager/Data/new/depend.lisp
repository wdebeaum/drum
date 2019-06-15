;;;;
;;;; W::depend
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	 (W::depend
	  (wordfeats (W::morph (:forms (-vb) :nom w::dependance)))
	  (SENSES
	   ((LF-PARENT ont::rely)
	    (example "I depended on you")
	    (TEMPL agent-neutral-XP-TEMPL (xp (% W::PP (W::ptype W::on))))
	    )
	   (
	    (lf-parent ont::rely-depend) ;; 20120523 GUM change new parent
	    (example "one thing depends on another")
	    (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
	    (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype W::on))))
	    )
	   (;;(LF-PARENT ont::rely)
	    (lf-parent ont::rely-depend) ;; 20120523 GUM change new parent
	    (example "it depends whether it works or not")
	    (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
	    (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% W::NP (W::sort W::wh-desc))))
	    )
	   )
   )
))

