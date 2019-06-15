;;;;
;;;; w::conclude
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (w::conclude
     (wordfeats (W::morph (:forms (-vb) :nom w::conclusion)))
   (SENSES
    ((LF-PARENT ONT::determine)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (example "we concluded that weaker conditions might promote sharing better")
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil :wn ("conclude%2:31:00"))
     )
    )
   )
))

