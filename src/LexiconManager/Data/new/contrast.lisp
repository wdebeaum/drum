;;;;
;;;; W::contrast
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::contrast
   (wordfeats (W::morph (:forms (-vb) :nom W::contrast)))
   (SENSES
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "contrast the options")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-NP-PLURAL-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "contrast this option with that option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )
    )
   )
))

