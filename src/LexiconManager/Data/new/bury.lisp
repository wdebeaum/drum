;;;;
;;;; W::bury
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::bury
    (wordfeats (W::morph (:forms (-vb) :nom w::burial)))
   (SENSES
    ((LF-PARENT ONT::put) ; like insert, embed
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("put-9.1"))
     (example "bury the chest")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

