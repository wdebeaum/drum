;;;;
;;;; W::steam
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::steam
   (SENSES
    ((LF-PARENT ONT::cloud-like-object)
     (templ mass-pred-templ)
     (meta-data :origin mobius :entry-date 20060712 :change-date nil :comments engine-texts)
     )
    )
   )
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::steam
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cook-in-steam)
   (example "steam the vegetables")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
   )
   ((meta-data :origin cardiac :entry-date 20081005 :change-date nil :comments nil)
   (LF-PARENT ONT::cook-in-steam)
   (example "the vegetables steamed")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (f::time-span f::extended))
   (templ affected-templ)
   )
  )
 )
))

