;;;;
;;;; W::reset
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::reset
   (wordfeats (W::morph (:forms (-vb) :past W::reset :ing W::resetting)))
   (SENSES
    ((meta-data :origin plow :entry-date 20050401 :change-date nil :comments nil)
     (LF-PARENT ONT::change-state-action)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (templ agent-affected-optional-templ (xp (% W::NP)))
     (example "reset the browser")
     )
    )
   )
))

