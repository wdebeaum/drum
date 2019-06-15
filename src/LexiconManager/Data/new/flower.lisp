;;;;
;;;; W::flower
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::flower
   (SENSES
    ((LF-PARENT ont::plant)
     (meta-data :origin calo-ontology :entry-date 20060712 :change-date nil :wn ("flower%1:20:00") :comment caloy3)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::flower
   (wordfeats (W::morph (:forms (-vb) :past W::flowered :ing W::flowering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("flower%2:30:00"))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (LF-PARENT ONT::bear)
 ; like ferment
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::bear)
     (example "the plants flowered")
     (templ agent-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

