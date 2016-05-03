;;;;
;;;; W::HELP
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::HELP
   (wordfeats (W::morph (:forms (-vb) :nom W::help)))
   (SENSES
    #||((LF-PARENT ONT::HELP)
     (example "I want to help")
     (TEMPL agent-templ)
     (meta-data :origin lam :entry-date 20050713 :change-date nil )
     )||#

    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("help-70-1"))
     (LF-PARENT ONT::HELP)
     (example "help the workers to load all the oranges")
     (TEMPL agent-EFFECT-affected-OBJCONTROL-TEMPL)
     )

    ((LF-PARENT ONT::HELP)
     (example "help the people help themselves")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-EFFECT-affected-OBJCONTROL-TEMPL (xp (% W::VP (W::vform W::base))))
     )

    ((LF-PARENT ONT::HELP)
     (example "I helped to load the oranges")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-EFFECT-SUBJCONTROL-TEMPL)
     )

    ((LF-PARENT ONT::HELP)
     (example "I helped load the oranges")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-EFFECT-SUBJCONTROL-TEMPL (xp (% W::VP (W::vform W::base))))
     )

    ((LF-PARENT ONT::HELP)
     (example "it helped me")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL agent-affected-xp-templ)
     )   
    )
   )
))

