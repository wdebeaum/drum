;;;;
;;;; W::repair
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::repair
   (SENSES
    ((LF-PARENT ONT::REPAIR)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (templ agent-affected-xp-templ)
     (example "repair the truck (with the wrench)")
     )
    )
   )
))

