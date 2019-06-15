;;;;
;;;; w::preheat
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::preheat
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::heat)
   (example "preheat the oven")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::heat-scale)))
   )
  ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
   (EXAMPLE "the oven preheated quickly")
   (LF-PARENT ONT::heat)
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::heat-scale))
   (TEMPL affected-unaccusative-templ)
   )
  )
 )
)

