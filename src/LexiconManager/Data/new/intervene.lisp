;;;;
;;;; W::intervene
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::intervene
   (wordfeats (W::morph (:forms (-vb) :nom w::intervention)))
   (SENSES
    (;(LF-PARENT ONT::hindering)
     (LF-PARENT ONT::manipulate)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL agent-affected-xp-templ) 
     (templ agent-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xxx W::in)))))
     (EXAMPLE "He intervened in the dispute")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Break-contact)
     )
    (;(LF-PARENT ONT::hindering)
     (LF-PARENT ONT::manipulate)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ) 
     (EXAMPLE "He intervened.")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Break-contact)
     )
    )
   )
))

