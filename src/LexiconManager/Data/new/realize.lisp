;;;;
;;;; W::realize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::realize
   (SENSES
    ;;;; I realized that the origional route is no longer available.
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
     (LF-PARENT ONT::come-to-understand)
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ;;;; I realize the risk
    ((LF-PARENT ONT::come-to-understand)
     (TEMPL agent-theme-xp-templ)
     )
    )
   )
))

