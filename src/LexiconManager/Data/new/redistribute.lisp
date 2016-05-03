;;;;
;;;; W::redistribute
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::redistribute
   (SENSES
    ((lf-parent ont::assign)
     (templ agent-affected-plural-recipient-optional-templ)
     (example "redistribute gifts to them")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil)
    )
    ((lf-parent ont::assign)
     (templ agent-affected-mass-recipient-optional-templ)
     (example "redistribute the software to them")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil)
    )
   )
  )
))

