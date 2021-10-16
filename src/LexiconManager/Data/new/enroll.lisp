;;;;
;;;; w::enroll
;;;;

(define-words :pos W::v 
 :words (
 (w::enroll
   (SENSES
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "enroll in the class")
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     ;(TEMPL AGENT-RESULT-SUBJCONTROL-OPTIONAL-TEMPL (xp (% W::ADVBL (w::lf (% w::prop (w::class ONT::SITUATED-IN))))))
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     ;(templ agent-affected-goal-optional-templ (xp (% W::ADVBL (w::lf (% w::prop (w::class ONT::SITUATED-IN))))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "enroll the student in the class")
     (meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :comments caloy3)
     )
    )
   )
))

