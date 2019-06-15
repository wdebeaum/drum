;;;;
;;;; W::FLOOD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FLOOD
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("flood%1:19:00"))
     (LF-PARENT ONT::flooding)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::flood
   (SENSES
    ((LF-PARENT ONT::flooding)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the river flooded")
     (templ affected-templ)
     )
    ((LF-PARENT ONT::flooding)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the river flooded the house")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

