;;;;
;;;; W::erode
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::erode
   (wordfeats (W::morph (:forms (-vb) :nom w::erosion)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("erode%2:30:01"))
     (templ agent-affected-xp-templ)
     (LF-PARENT ONT::deteriorate)
 ; like ferment
     )
   ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
    (LF-PARENT ONT::deteriorate)
    (example "the soil eroded")
    (templ affected-templ)
    ;(SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
    )
    )
   )
))

