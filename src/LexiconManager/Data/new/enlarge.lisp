;;;;
;;;; w::enlarge
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::enlarge
  (wordfeats (W::morph (:forms (-vb) :nom w::enlargement)))
 (senses
  ((meta-data :origin task-learning :entry-date 20050826 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::enlarge)
   (example "enlarge the image (to full screen)")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::scale ont::size-scale))
   ;(TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))));??
   (TEMPL AGENT-AFFECTED-XP-TEMPL)
   )

   ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
    (EXAMPLE "Fruit had brown lesions that enlarged quickly.")
    (LF-PARENT ONT::enlarge)
    (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))
    (TEMPL affected-templ)
    )
  )
 )
))

