;;;;
;;;; W::surrender
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::surrender
    (wordfeats (W::morph (:forms (-vb) :past W::surrendered :ing surrendering)))
   (SENSES
    ((lf-parent ont::surrender)
     (templ agent-affected-goal-optional-templ (xp (% W::pp (W::ptype W::to))))
     (example "surrender rights to him")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil)
     )
    ((lf-parent ont::surrender)
     (templ agent-affected-xp-templ (xp (% W::pp (W::ptype W::to))))
     (example "he surrendered")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil :wn ("surrender%2:40:00"))
     )
    )
   )
))

