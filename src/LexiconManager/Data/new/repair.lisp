;;;;
;;;; W::repair
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::repair
   (SENSES
    ((LF-PARENT ONT::REPAIR)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "repair the truck (with the wrench)")
     )
    )
   )
))

