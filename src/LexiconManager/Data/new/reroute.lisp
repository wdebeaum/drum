;;;;
;;;; W::reroute
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::reroute
   (SENSES
    ;;;; reroute the cargo to Avon
    ((LF-PARENT ONT::SEND)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

