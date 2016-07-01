;;;;
;;;; w::constrict
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
   (w::constrict
   (wordfeats (W::morph (:forms (-vb) :nom w::constriction)))
   (senses
    ((meta-data :origin "verbnet2.0" :entry-date 20060606 :change-date 20090504 :comments nil :vn ("other_cos-45.4") :wn ("tighten%2:30:01" "tighten%2:30:00"))
     (LF-PARENT ONT::decrease)
     (syntax (w::resultative +)) ;; the tightened screw
     (example "the chains constricted his movement")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::decrease)
     (templ affected-templ)
     (example "the arteries constricted")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

