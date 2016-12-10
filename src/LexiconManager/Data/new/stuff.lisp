;;;;
;;;; w::stuff
;;;;

(define-words :pos W::n
 :words (
  (w::stuff
  (senses
   ((LF-PARENT ONT::substance)
    (TEMPL mass-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    (example "high cholesterol")
    )
   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::stuff
   (SENSES
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-affected-goal-templ (xp (% W::PP (W::ptype (? t W::into W::in)))))
     (example "stuff the people in/into the rescue vehicle (like sardines)")
     )
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "load the truck with oj")
     (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

