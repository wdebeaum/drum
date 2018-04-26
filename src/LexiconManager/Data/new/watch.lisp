;;;;
;;;; W::watch
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
;; seems like this should be an agent instead of experiencer, but ont::active-perception also seems good
 (W::watch
   (SENSES
    ((LF-PARENT ONT::scrutiny)
     (example "watch it")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-NEUTRAL-optional-TEMPL)
     )
    ((lf-parent  ont::taking-care-of)
     ;(lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he is watching his weight")
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     )

    ((lf-parent ont::wait-watch)
     (example "He is watching for the boat to arrive")
     (TEMPL agent-action-templ)
     )

    ((lf-parent ont::wait-watch)
     (example "He is watching for the boat")
     (TEMPL agent-neutral-xp-templ (xp (% W::PP (W::ptype w::for)))
	    )
    
     )
    )
   )
 ))

