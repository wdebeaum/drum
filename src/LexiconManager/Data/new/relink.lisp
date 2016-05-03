;;;;
;;;; W::relink
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::relink
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype W::to))))
     (EXAMPLE "relink the program to the library")
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments :nil)
     )
    )
   )
))

