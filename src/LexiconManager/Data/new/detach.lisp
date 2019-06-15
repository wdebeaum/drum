;;;;
;;;; W::detach
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::detach
   (SENSES
    ((LF-PARENT ont::unattach)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "detach the file")
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Break-contact)
     )
     ((LF-PARENT ont::unattach)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "detach from the group")
     (TEMPL AGENT-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (meta-data :origin coordops :entry-date 20070706 :change-date nil :comments nil)
     )

    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     (example "They detached")
     )

    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::detach
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("detach%2:35:01"))
     (LF-PARENT ONT::unattach)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))

