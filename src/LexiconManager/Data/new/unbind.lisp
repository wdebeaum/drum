;;;;
;;;; W::unbind
;;;;

(define-words :pos W::v 
 :words (
  (W::unbind
   (wordfeats (W::morph (:forms (-vb) :past W::unbound :pastpart w::unbound)))
   (SENSES

    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? xx W::from)))))
     (example "We unbind the patient from the stretcher")
     )
 
    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xxx W::from)))))
     (example "It unbinds from the stretcher")
     )

    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-NP-PLURAL-TEMPL)
     (example "They unbind")
     )

    )
   )

))
