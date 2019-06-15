;;;;
;;;; W::stagnate
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::stagnate
   (SENSES
; BAD AUTOMATICALLY ADDED SENSE more like lack of transformation
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5"))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (LF-PARENT ONT::deteriorate)
 ; like ferment
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::deteriorate)
     (example "the economy stagnated")
     (templ affected-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

