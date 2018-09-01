;;;;
;;;; W::heat
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::heat
   (SENSES
    ((LF-PARENT ONT::heat-scale)
     (example "oppressive heat is forecast for today")
     (meta-data :origin plow :entry-date 20060802 :change-date nil :comments weather :wn ("heat%1:07:01" "heat%1:09:00"))
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
(w::heat
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::heat)
   (example "heat the water")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::heat-scale))
   )
  ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
   (EXAMPLE "water heated in the sun")
   (LF-PARENT ONT::heat)
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::heat-scale))
   (TEMPL affected-unaccusative-templ)
   )
  )
 )
))

