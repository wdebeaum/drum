;;;;
;;;; W::stylize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::stylize
   (SENSES
    ((lf-parent ont::language-adjust)
     (example "stylize the message")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-theme-xp-templ)
     (meta-data :origin task-learning :entry-date 20050919 :change-date 20090504 :comments nil)
     )
    )
   )
))

