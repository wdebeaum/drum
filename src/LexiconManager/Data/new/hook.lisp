;;;;
;;;; W::hook
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::hook (W::on))
   (SENSES
    ;;;; hook the car on the train????
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::hook (W::up))
   (SENSES
    ;;;; hook up the car to the train
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;; Myrosia fixed 20040704 - used the incorrect template - needs 3 arguments, with optional pp!!! 
     (TEMPL AGENT-affected2-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

