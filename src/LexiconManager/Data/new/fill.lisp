;;;;
;;;; W::fill
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::fill
   (SENSES
    ((LF-PARENT ONT::FILL-container)
     (example "fill the truck with oj")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
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
      (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-2-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::with)))))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
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

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::fill (W::in))
   (SENSES
     ((LF-PARENT ONT::enter-on-form)
      (meta-data :origin calo :entry-date 20040622 :change-date nil :comments y2)
      (example "fill in the requisition form")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
     ((LF-PARENT ONT::write)
      (meta-data :origin plow :entry-date 20080924 :change-date nil :comments nil)
      (example "fill in the place of employment")
      (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
      (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

