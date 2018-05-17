;;;;
;;;; W::PARK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PARK
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::region-for-activity)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::PARK
   (SENSES
    ((EXAMPLE "He parked the truck")
     (LF-PARENT ONT::place-in-position)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    
   ))
))

