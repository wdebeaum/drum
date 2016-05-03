;;;;
;;;; W::unplug
;;;;

(define-words :pos W::v 
 :words (
  (W::unplug
   (SENSES
    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "unplug the ethernet cable")
      (TEMPL AGENT-affected-SOURCE-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (meta-data :origin task-learning :entry-date 20050912 :change-date nil :comments nil)
     )
    )
   )
))

