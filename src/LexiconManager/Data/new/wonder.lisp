;;;;
;;;; W::wonder
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::wonder
   (wordfeats (W::morph (:forms (-vb) :past W::wondered :ing w::wondering :nom w::wonder)))
   (SENSES
    ;;;; I wonder
    ((LF-PARENT ONT::cogitation)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    ;;;; I wonder if it will work
    ((LF-PARENT ONT::cogitation)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-if))))
     )
    )
   )
))

