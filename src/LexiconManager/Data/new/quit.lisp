;;;;
;;;; W::quit
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::quit
   (wordfeats (W::morph (:forms (-vb) :past W::quit :ing W::quitting)))
   (SENSES
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the engine/program quit")
     (TEMPL affected-TEMPL)
     )
    ((LF-PARENT ONT::STOP)
     (example "quit complaining")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-subjcontrol-templ (xp (% W::VP (W::vform W::ing))))
     )
    ((meta-data :origin plot :entry-date 20080413 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ)
     (example "quit the \"pine \" program")
     )
    )
   )
))

