;;;;
;;;; w::preheat
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::preheat
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::heat)
   (example "preheat the oven")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

