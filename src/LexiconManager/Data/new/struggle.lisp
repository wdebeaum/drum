;;;;
;;;; w::struggle
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
       (w::struggle
     (senses
      ((lf-parent ont::fighting)
       (example "he struggled with/for/against the proposal")
       (templ agent-templ)
       (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
       )
       ((lf-parent ont::fighting)
       (example "he struggled to breath")
       (TEMPL AGENT-theme-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
       (meta-data :origin cardiac :entry-date 20080630 :change-date nil :comments nil)
       )
      ))
))

