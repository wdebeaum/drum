;;;;
;;;; W::blossom
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::blossom
   (wordfeats (W::morph (:forms (-vb) :past W::blossomed :ing W::blossoming)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("blossom%2:30:01"))
     (templ agent-affected-xp-templ)
     (LF-PARENT ONT::life-transformation)
 ; like ferment
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::life-transformation)
     (example "the trees blossomed")
     (templ affected-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

