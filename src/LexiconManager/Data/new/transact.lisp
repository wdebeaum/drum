;;;;
;;;; W::transact
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::transact
    (wordfeats (W::morph (:forms (-vb) :nom W::transaction)))
   (SENSES
    ((LF-PARENT ONT::INTERACT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-CO-AGENT-OPTIONAL-TEMPL )
     (example "The cat transacted business with the mouse.")
     )
    )
   )
))

