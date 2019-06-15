;;;;
;;;; W::hook
;;;;

(define-words :pos W::v
 :words (
  ((W::hook)
   (SENSES
    ;;;; hook (up) the car to the train
    ;;;; hook the car on the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;; Myrosia fixed 20040704 - used the incorrect template - needs 3 arguments, with optional pp!!! 
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? yy W::to W::on)))))
     )
    )
   )
))
