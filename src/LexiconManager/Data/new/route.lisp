;;;;
;;;; W::ROUTE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ROUTE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("route%1:06:00"))
     (LF-PARENT ONT::route)
     (prototypical-word t)
     (SEM (F::information F::information-content))
     )
    ((meta-data :origin ralf :entry-date 20040802 :change-date nil :wn ("route%1:15:00") :comments nil)
     (LF-PARENT ONT::ROUTE)
     (example "the route of the truck")
     (TEMPL OTHER-RELN-of-for-TEMPL)
     (prototypical-word t)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::ROUTE
   (SENSES
    ;;;; route the cargo to Avon
    ((LF-PARENT ONT::SEND)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

