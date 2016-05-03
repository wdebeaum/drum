;;;;
;;;; W::replace
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::replace
   (wordfeats (W::morph (:forms (-vb) :nom W::replacement)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::Replacement)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     )
    ((meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1)
     (LF-PARENT ONT::Replacement)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (example "replace the triangle with the square")
     (TEMPL agent-affected2-templ (xp (% W::PP (W::ptype W::with))))
     )
    )
   )
))

