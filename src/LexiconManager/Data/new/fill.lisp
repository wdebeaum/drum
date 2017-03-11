;;;;
;;;; W::fill
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::fill
   (SENSES
    ((LF-PARENT ONT::FILL-container)
     (example "fill the truck with oj")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    ((LF-PARENT ONT::FILL-container)
     (example "the container filled")
     (TEMPL affected-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::fill (W::up))
   (SENSES
     ((LF-PARENT ONT::FILL-container)
      (example "fill up the truck with oj")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      (TEMPL AGENT-GOAL-affected-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::fill (W::out))
   (SENSES
     ((LF-PARENT ONT::enter-on-form)
      (meta-data :origin calo :entry-date 20040622 :change-date nil :comments y2)
      (example "fill out the requisition form")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::fill (W::in))
   (SENSES
     ((LF-PARENT ONT::enter-on-form)
      (meta-data :origin calo :entry-date 20040622 :change-date nil :comments y2)
      (example "fill in the requisition form")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      (TEMPL AGENT-affected-XP-TEMPL)
     )
     ((LF-PARENT ONT::write)
      (meta-data :origin plow :entry-date 20080924 :change-date nil :comments nil)
      (example "fill in the place of employment")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      (TEMPL AGENT-affected-XP-TEMPL)
     )
    )
   )
))

