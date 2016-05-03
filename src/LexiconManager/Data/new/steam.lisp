;;;;
;;;; W::steam
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::steam
   (SENSES
    ((LF-PARENT ONT::cloud-object)
     (templ mass-pred-templ)
     (meta-data :origin mobius :entry-date 20060712 :change-date nil :comments engine-texts)
     )
    )
   )
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::steam
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "steam the vegetables")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
   ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::cooking)
   (example "the vegetables steamed")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
   (templ affected-templ)
   )
  )
 )
))

