;;;;
;;;; W::restart
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::restart
   (SENSES
    ((LF-PARENT ONT::RESUME) ;RESTART)
     (example "restart")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ)
     )
    ((LF-PARENT ONT::RESUME) ;RESTART)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "restart the action")
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     )
    ((LF-PARENT ONT::RESTART)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "restart the computer")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

