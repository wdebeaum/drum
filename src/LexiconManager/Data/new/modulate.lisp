;;;;
;;;; W::modulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::modulate
   (wordfeats (W::morph (:forms (-vb) :nom w::modulation :agentnom w::modulator)))
   (SENSES
    (;(LF-PARENT ONT::modify)
     (LF-PARENT ONT::manage)
     (example "modulate the signal")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date 20090504 :comments Break-contact)
     )
    )
   )
))

