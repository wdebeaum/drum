;;;;
;;;; W::exchange
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::exchange
   (wordfeats (W::morph (:forms (-vb) :nom W::exchange)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::exchange)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     )
    ((LF-PARENT ONT::exchange)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (example "exchange the triangle with the square")
     (TEMPL agent-affected2-templ (xp (% W::PP (W::ptype (? pt w::for W::with)))))
     )
    )
   )
))

