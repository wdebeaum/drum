;;;;
;;;; W::wonder
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::wonder
   (wordfeats (W::morph (:forms (-vb) :past W::wondered :ing w::wondering :nom w::wonder)))
   (SENSES
    ;;;; I wonder
    ((LF-PARENT ONT::cogitation)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     )
    ;;;; I wonder if it will work
    ((LF-PARENT ONT::cogitation)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-if))))
     )
    )
   )
))

