;;;;
;;;; W::try
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::try   
   (wordfeats (W::morph (:forms (-vb) :nom w::try)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("try%2:37:01"))
     (LF-PARENT ONT::evoke-anger)
     (example "the strenuous trial tried his patience")
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("try-61"))
     (LF-PARENT ONT::TRY)
     (example "try this option")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    ((LF-PARENT ONT::TRY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "try going along the coast")
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::VP (W::vform W::ing))))
     )
    ((LF-PARENT ONT::TRY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype (? ct W::s-to W::s-and)))))
     (example "try to go around the storm")
     )
    ((LF-PARENT ONT::TRY)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-templ)
     (example "He tried")
     (preference 0.96) ;; use only if necessary
     )
    )
   )
))

