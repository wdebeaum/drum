;;;;
;;;; W::vote
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::vote
   (SENSES
    ((LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "who voted for/against Bush" "vote on the proposition")
      (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype (? pt w::for w::on w::against))))) 
     )
     ((LF-PARENT ONT::SELECT)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the committee voted this morning")
     (TEMPL agent-templ) 
     )
    )
   )
))

