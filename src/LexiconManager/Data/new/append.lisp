;;;;
;;;; W::append
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::append
   (SENSES
    ((LF-PARENT ONT::combine-objects)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     (example "append the messages to this message")
     )
    )
   )
))

