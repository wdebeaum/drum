;;;;
;;;; W::orbit
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::orbit
   (wordfeats (W::morph (:forms (-vb) :past W::orbited :ing w::orbiting :nom w::orbit)))
   (SENSES
    ((LF-PARENT ONT::FLY)
     (example "orbit over the landscape")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (templ agent-templ)
     )
    ((LF-PARENT ONT::FLY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "orbit the earth")
     )
    )
   )
))

