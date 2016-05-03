;;;;
;;;; W::quelch
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::quelch
   (SENSES
    ((LF-PARENT ONT::STOP)
     (example "they quelched the rebellion")
     (templ agent-affected-xp-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

