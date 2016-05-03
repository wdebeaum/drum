;;;;
;;;; W::stow
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::stow
   (SENSES
    ((EXAMPLE "I am starting to stow the equipment")
     (LF-PARENT ONT::PUT-AWAY)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

