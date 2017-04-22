;;;;
;;;; W::invert
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::invert
   (SENSES
    ((LF-PARENT ONT::move-upside-down)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "invert the vase")
     )
    )
   )
))

