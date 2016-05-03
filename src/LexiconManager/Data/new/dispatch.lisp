;;;;
;;;; W::dispatch
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::dispatch
   (SENSES   
    ((LF-PARENT ONT::SEND)
     (example "dispatch the truck/the cargo to Avon")
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

