;;;;
;;;; W::restart
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::restart
   (SENSES
    ((LF-PARENT ONT::RESTART)
     (example "restart")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ)
     )
    ((LF-PARENT ONT::RESTART)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "restart the action")
     (templ agent-effect-xp-templ)
     )
    ((LF-PARENT ONT::RESTART)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "restart the computer")
     (templ agent-affected-xp-templ)
     )
    )
   )
))

