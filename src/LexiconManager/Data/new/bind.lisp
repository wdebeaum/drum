;;;;
;;;; W::bind
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::bind
   (wordfeats (W::morph (:forms (-vb) :past W::bound :pastpart w::bound)))
   (SENSES

    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? xx W::to w::with)))))
     (example "We bind the patient to the stretcher")
     )
 
    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xxx W::to W::with)))))
     (example "It binds to the stretcher")
     )

    ((LF-PARENT ONT::ATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     (example "They bind together")
     )

    )
   )

))

