;;;;
;;;; W::revolt
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::revolt
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("revolt%2:37:00" "revolt%2:39:00"))
     (LF-PARENT ONT::evoke-disgust)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::V
 :words (
  (W::revolt
   (wordfeats (W::morph (:forms (-vb) :nom w::revolt)))
   (SENSES
    (
     (LF-PARENT ONT::GROUP-CONFLICT)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

