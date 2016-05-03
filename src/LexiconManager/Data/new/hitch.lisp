;;;;
;;;; W::hitch
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::hitch (W::along))
   (SENSES
    ;;;; hitch the truck along the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::hitch
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (example "hitch the boxcar to the engine")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

