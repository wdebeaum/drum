;;;;
;;;; W::interrupt
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::interrupt
   (wordfeats (W::morph (:forms (-vb) :nom w::interruption)))
   (SENSES
    ((LF-PARENT ONT::hindering)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ) 
     (EXAMPLE "The storm interrupted the activity/signal")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Break-contact)
     )
    )
   )
))

