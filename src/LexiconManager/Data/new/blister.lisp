;;;;
;;;; W::blister
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::blister
   (wordfeats (W::morph (:forms (-vb) :past W::blistered :ing W::blistering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("blister%2:30:00"))
     (LF-PARENT ONT::swell)
     (example "The sun blistered my feet")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
 ; like ferment
     )
   ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
    (LF-PARENT ONT::swell)
    (example "his skin blistered")
    (templ affected-templ)
    (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))
    )
    )
   )
))

