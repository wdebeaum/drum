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

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::stuff
   (SENSES
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL (xp (% W::PP (W::ptype (? t W::into W::in)))))
     (example "stuff the people in/into the rescue vehicle (like sardines)")
     )
    ((LF-PARENT ONT::Fill-container)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "load the truck with oj")
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

