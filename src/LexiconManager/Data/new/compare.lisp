;;;;
;;;; W::compare
;;;;

(define-words :pos W::v 
 :words (
 (W::compare
   (wordfeats (W::morph (:forms (-vb) :nom W::comparison)))
   (SENSES
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "compare the options")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-NP-PLURAL-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "compare this option with/to that option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::with w::to w::for)))))
     )
    ((meta-data :origin calo :entry-date 20060306 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "compare these computers on price")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP1-3-XP2-TEMPL (xp2 (% W::PP (W::ptype (? pt w::on w::by)))))
     )
    ((meta-data :origin calo :entry-date 20050502 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "how does this option compare with/to that option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype (? pt W::with w::to)))))
     )
    ((meta-data :origin calo :entry-date 20060306 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "how do these computers compare on price")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt w::on w::by)))))
     )
    ;; how does it compare to this on price
    )
   )
))

