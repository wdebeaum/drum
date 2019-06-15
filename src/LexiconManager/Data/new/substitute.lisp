;;;;
;;;; W::substitute
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
   (W::substitute
   (SENSES
    ((LF-PARENT ont::replacement)
     (meta-data :origin calo :entry-date 20050527 :comments projector-purchasing)
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::for)))))
     (example "I'm looking for a substitute for my old projector")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::substitute
    (wordfeats (W::morph (:forms (-vb) :nom W::substitution)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::Replacement)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     )
    ((meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1 :vn ("exchange-13.6-1-1"))
     (LF-PARENT ONT::Replacement)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (example "substitute the triangle for the square")
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype (? pt w::with W::for)))))
     )
    )
   )
))

