;;;;
;;;; w::resize
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::resize
 (senses
  ((meta-data :origin task-learning :entry-date 20050815 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::change-magnitude)
   (example "resize and position the image")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  )
 )
))

