;;;;
;;;; W::finish
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::finish
   (SENSES
    ;;;; swier -- Finish the plan
    ((LF-PARENT ONT::complete)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ)
     )
    ;;;; they finished
    ((LF-PARENT ONT::COMPLETE)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)
     )
    ;;;; Finish doing the action
    ((LF-PARENT ONT::COMPLETE)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::VP (W::vform W::ing))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::finish (W::up))
   (SENSES
    ;;;; swier -- Finish the plan
    ((LF-PARENT ONT::COMPLETE)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ;;;; they finished up
    ((LF-PARENT ONT::COMPLETE)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    ;;;; Finish doing the action
    ((LF-PARENT ONT::COMPLETE)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::VP (W::vform W::ing))))
     )
    )
   )
))

