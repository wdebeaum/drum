;;;;
;;;; W::integrate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::integrate
   (SENSES
    ((EXAMPLE "integrate mail with other applications")
     (LF-PARENT ont::add-include)
     (TEMPL AGENT-AFFECTED-RESULT-TO-OBJCONTROL-TEMPL (xp (% W::PP (W::ptype (? pt w::into w::in W::with)))))
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090908 :comments nil)
     )
    ; more senses?
    )
   )
))

