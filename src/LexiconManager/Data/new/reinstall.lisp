;;;;
;;;; w::reinstall
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::reinstall
 (senses
  ((lf-parent ont::set-up-device)
   (example "reinstall the system")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL agent-theme-xp-templ)
   (meta-data :origin task-learning :entry-date 20050912 :change-date 20090504 :comments nil)
   )
  )
 )
))

