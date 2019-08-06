;;;;
;;;; W::neglect
;;;;

(define-words :pos W::v 
 :words (
   (W::neglect
   (SENSES
    ((lf-parent ont::fail)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he neglected to water the plants")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to))))
     (meta-data :origin calo-ontology :entry-date 20060710 :change-date nil :comments caloy3)
     )
    ((LF-PARENT ONT::ignore)
     (example "he neglected the problem")
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-neutral-XP-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060710 :change-date nil :comments caloy3)
     )
    ))
))

