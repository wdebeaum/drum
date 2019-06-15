;;;;
;;;; W::molt
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::molt
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("molt%2:29:00"))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (LF-PARENT ONT::life-transformation)
 ; like ferment
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::life-transformation)
     (example "the snake molted")
     (templ affected-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

