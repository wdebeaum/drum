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
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype (? xx W::from)))))
     (example "We unbind the patient from the stretcher")
     )
 
    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xxx W::from)))))
     (example "It unbinds from the stretcher")
     )

    ((LF-PARENT ONT::UNATTACH)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-plural-TEMPL)
     (example "They unbind")
     )

    )
   )

))
