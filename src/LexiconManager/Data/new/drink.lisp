;;;;
;;;; W::drink
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::drink
   (wordfeats (W::morph (:forms (-vb) :past W::drank :pastpart W::drunk :ing W::drinking)))
   (SENSES
    ((EXAMPLE "Drink water")
     (LF-PARENT ONT::drink)
     (syntax (w::resultative +))
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((EXAMPLE "Don't drink")
     (LF-PARENT ONT::drink)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (PREFERENCE 0.98)
     )
    )
   )
))

