;;;;
;;;; w::agree
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::agree
	   (wordfeats (W::morph (:forms (-vb) :past W::agreed :ing W::agreeing)))
	   (senses
	    ((LF-parent ont::correlation)
	     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
	     (Example "This evidence agrees with our findings")
	     ;(TEMPL neutral-theme-xp-templ (xp (% w::PP (w::ptype w::with))))
	     ;(TEMPL neutral-formal-as-comp-templ (xp (% w::PP (w::ptype w::with))))
	     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% w::PP (w::ptype w::with))))
	     (meta-data :origin bee :entry-date 20040607 :change-date 20090604 :comments portability-expt) 
	     )	    
	    ((LF-parent ont::correlation)
	     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
	     (Example "The results agree that the theory is true.")
	     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
	     )	
	    ((LF-parent ont::accept-agree)
	     (Example "They agreed that the theory is true.")
	     (TEMPL AGENT-FORMAL-XP-PP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
	     )
	    (;(LF-parent ont::accept)
	     (lf-parent ont::accept-agree) ;; 20120523 GUM change new parent
	     (Example "He agreed [with John]")
	     (TEMPL AGENT-AGENT1-XP-PP-OPTIONAL-TEMPL )
	     (meta-data :origin bee :entry-date 20040607 :change-date 20090508 :comments portability-expt) 
	     )
	    (;(LF-parent ont::accept)
	     (lf-parent ont::accept-agree) ;; 20120523 GUM change new parent
	     (Example "He agreed with the findings")
	     (TEMPL AGENT-neutral-XP-TEMPL (xp (% W::PP (W::ptype (? xx W::to W::on w::with)))))
	     )
	  
	    )))
 )
