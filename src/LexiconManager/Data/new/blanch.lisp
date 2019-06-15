;;;;
;;;; w::blanch
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::blanch
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::boil)
   (example "blanch the almonds")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::boil)
   (example "the almonds blanched")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   (templ affected-templ)
   )
  )
 )
))

