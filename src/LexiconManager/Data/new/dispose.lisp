;;;;
;;;; W::dispose
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::dispose
   (wordfeats (W::morph (:forms (-vb) :nom w::disposal)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date 20090529 :comments s11)
     (LF-PARENT ONT::discard)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL (xp (% W::PP (W::ptype W::of))))
     (example "dispose of something")
     )
    )
   )
))

