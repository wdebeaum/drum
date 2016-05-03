;;;;
;;;; W::extricate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::extricate
    (wordfeats (W::morph (:forms (-vb) :nom W::extrication)))
   (SENSES
    ((LF-PARENT ONT::releasing)
     (example "extricate the people")
     (meta-data :origin pacifica :entry-date unknown :change-date 20090529 :comments change-lf)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

