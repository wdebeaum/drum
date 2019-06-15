;;;;
;;;; w::simmer
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::simmer
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::boil)
   (example "simmer the chicken")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
   ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::boil)
   (example "the chicken simmered")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (f::time-span f::extended))
   (templ affected-templ)
   )
  )
 )
))

