;;;;
;;;; W::interrupt
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::interrupt
   (wordfeats (W::morph (:forms (-vb) :nom w::interruption)))
   (SENSES
    ((LF-PARENT ONT::stop)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (EXAMPLE "The storm interrupted the activity/signal")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Break-contact)
     )
    ((LF-PARENT ONT::stop)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ) 
     (EXAMPLE "sorry I interrupted")
     )
    )
   )))

