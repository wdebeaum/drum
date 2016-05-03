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
     (TEMPL agent-neutral-plural-templ)
     )
    ((meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "compare this option with/to that option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral2-TEMPL (xp (% W::PP (W::ptype (? pt W::with w::to w::for)))))
     )
    ((meta-data :origin calo :entry-date 20060306 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "compare these computers on price")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-theme-TEMPL (xp2 (% W::PP (W::ptype (? pt w::on w::by)))))
     )
    ((meta-data :origin calo :entry-date 20050502 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "how does this option compare with/to that option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-TEMPL (xp (% W::PP (W::ptype (? pt W::with w::to)))))
     )
    ((meta-data :origin calo :entry-date 20060306 :change-date nil :comments caloy2)
     (LF-PARENT ONT::compare)
     (example "how do these computers compare on price")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL neutral-formal-optional-TEMPL (xp (% W::PP (W::ptype (? pt w::on w::by)))))
     )
    ;; how does it compare to this on price
    )
   )
))

