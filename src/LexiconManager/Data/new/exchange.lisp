;;;;
;;;; W::exchange
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
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
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype (? pt w::for W::with)))))
     )
    )
   )
))

