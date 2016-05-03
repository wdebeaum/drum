;;;;
;;;; W::bind
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::bind
   (wordfeats (W::morph (:forms (-vb) :past W::bound :pastpart w::bound)))
   (SENSES

    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype (? xx W::to w::with)))))
     (example "We bind the patient to the stretcher")
     )
 
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xxx W::to W::with)))))
     (example "It binds to the stretcher")
     )

    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-plural-TEMPL)
     (example "They bind together")
     )

    )
   )

))

