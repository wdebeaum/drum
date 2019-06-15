;;;;
;;;; W::redistribute
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::redistribute
   (SENSES
    ((lf-parent ont::assign)
     (TEMPL AGENT-AFFECTED-RESULT-2-NP-PLURAL-3-OPTIONAL-TEMPL)
     (example "redistribute gifts to them")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil)
    )
    ((lf-parent ont::assign)
     (TEMPL AGENT-AFFECTED-RESULT-2-NP-MASS-3-OPTIONAL-TEMPL)
     (example "redistribute the software to them")
     (meta-data :origin task-learning :entry-date 20050831 :change-date 20090501 :comments nil)
    )
   )
  )
))

