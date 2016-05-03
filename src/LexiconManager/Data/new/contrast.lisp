;;;;
;;;; W::contrast
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::contrast
   (wordfeats (W::morph (:forms (-vb) :nom W::contrast)))
   (SENSES
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "contrast the options")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-plural-templ)
     )
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "contrast this option with that option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral2-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    )
   )
))

