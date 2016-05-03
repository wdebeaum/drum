;;;;
;;;; W::reformulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::reformulate
   (SENSES
    ;;;; reformulate the plan
    ((LF-PARENT ONT::REVISE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

