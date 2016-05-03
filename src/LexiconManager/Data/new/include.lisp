;;;;
;;;; W::include
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::include
 (wordfeats (W::morph (:forms (-vb) :nom w::inclusion)))
 (SENSES
  ((EXAMPLE "it includes a wireless card")
   (LF-PARENT ONT::HAVE)
   (SEM (F::Aspect F::static) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )
    ((EXAMPLE "include a wireless card in the order")
     (meta-data :origin calo :entry-date 20050425 :change-date 20090908 :comments projector-purchasing)
     (LF-PARENT ont::add-include)
     (TEMPL agent-affected-goal-to-TEMPL (xp (% W::PP (W::ptype (? pt W::in W::with)))))
     )
    )
 )
))
