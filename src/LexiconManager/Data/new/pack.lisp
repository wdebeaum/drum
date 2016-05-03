;;;;
;;;; W::pack
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::pack
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::Fill-container)
     (example "pack the oranges into the boxcar")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-goal-optional-templ (xp (% W::PP (W::ptype (? t w::on W::onto W::into W::in)))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("spray-9.7-1-1"))
     (LF-PARENT ONT::Fill-container)
     (example "pack the truck with oranges")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::pack (W::up))
   (SENSES
    ;;;; swier -- pack up the oranges
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    ;;;; added by swier
    ;;;; swier -- pack up the truck with oranges
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

