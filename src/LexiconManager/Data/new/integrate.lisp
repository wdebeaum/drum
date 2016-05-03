;;;;
;;;; W::integrate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::integrate
   (SENSES
    ((EXAMPLE "integrate mail with other applications")
     (LF-PARENT ont::add-include)
     (TEMPL AGENT-affected-GOAL-to-TEMPL (xp (% W::PP (W::ptype (? pt w::into w::in W::with)))))
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090908 :comments nil)
     )
    ; more senses?
    )
   )
))

