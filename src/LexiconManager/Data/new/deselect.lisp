;;;;
;;;; W::deselect
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::deselect
   (SENSES
    ((LF-PARENT ONT::DESELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "deselect that checkbox")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :comments nil)
     )
    ((meta-data :origin plow :entry-date 20060531 :change-date nil :comments nil :vn ("characterize-29.2") :wn ("select%2:31:00"))
     (LF-PARENT ONT::DESELECT)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "deselect red")
     (templ agent-theme-pred-templ)
     )
    )
   )
))

