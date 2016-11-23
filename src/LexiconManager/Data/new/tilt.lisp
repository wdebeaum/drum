;;;;
;;;; W::tilt
;;;;

(define-words :pos W::n :templ count-pred-templ
 :words (
	  (W::tilt
	  (SENSES
	   ((LF-PARENT ONT::leaning)
	    (example "the robot's tilt sensor went off")
	    (meta-data :origin joust :entry-date 20091027 :change-date nil :comments nil :wn nil)
	    )
	   ))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (w::tilt
   (senses
    ((LF-PARENT ONT::place-in-position)
     (meta-data :origin lou :entry-date 20041111 :change-date nil :comments lou-sent-entry)
     (example "tilt the camera up a little")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
     ((LF-PARENT ONT::place-in-position)
     (meta-data :origin lou :entry-date 20041111 :change-date nil :comments lou-sent-entry)
     (example "tilt up a little")
     (templ agent-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

