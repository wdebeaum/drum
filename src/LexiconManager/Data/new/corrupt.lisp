;;;;
;;;; W::corrupt
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::corrupt
   (SENSES
    #||((LF-PARENT ONT::BREAK-OBJECT)  ;; subsumed by CAUSE template
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he corrupted the file")
     )||#
    (;(LF-PARENT ont::break-object)
     (LF-PARENT ont::damage)
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
     (SEM (F::Cause F::agentive) ;(F::Cause F::Phenomenal)
	  (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ)
     (example "it corrupted the file")
     )
    )
   )
))

