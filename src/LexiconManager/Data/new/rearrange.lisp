;;;;
;;;; W::rearrange
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::rearrange
   (SENSES
    ((lf-parent ont::arranging)
     (example "rearrange them into groups of three")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::into))))
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil :vn ("create-26.4-1"))
     )
    )
   )
))

