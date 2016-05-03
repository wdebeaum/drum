;;;;
;;;; W::bypass
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::bypass
   (SENSES
    ((LF-PARENT ONT::MOVE-around)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

