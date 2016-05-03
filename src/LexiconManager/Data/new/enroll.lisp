;;;;
;;;; w::enroll
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (w::enroll
   (SENSES
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "enroll in the class")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (templ agent-goal-optional-templ (xp (% W::PP (W::ptype (? t W::for W::in)))))
     )
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "enroll the student in the class")
     (meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :comments caloy3)
     )
    )
   )
))

