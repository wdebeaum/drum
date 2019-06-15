;;;;
;;;; W::quelch
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::quelch
   (SENSES
    ((LF-PARENT ONT::STOP)
     (example "they quelched the rebellion")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

