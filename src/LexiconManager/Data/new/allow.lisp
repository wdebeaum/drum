;;;;
;;;; W::allow
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::allow
   (SENSES
    ((LF-PARENT ONT::ALLOW)
     (TEMPL agent-EFFECT-affected-OBJCONTROL-TEMPL)
     (example "allow the team by the lake to go home")
     )
    ((meta-data :origin plow :entry-date 20060414 :change-date nil :comments nil :vn ("allow-64"))
     (LF-PARENT ONT::ALLOW)
     (TEMPL agent-affected-xp-templ)
     (example "Cockroaches are not allowed")
     )
     (;;(LF-PARENT ONT::ACCOMMODATE)
      (lf-parent ont::accommodate-allow) ;; 20120524 GUM change new parent
      (example "the budget allows for a thousand dollars")
      (TEMPL neutral-neutral-templ (xp (% W::pp (W::ptype W::for))))
      (meta-data :origin calo :entry-date 20040507 :change-date nil :comments y1-variations)
     )
    )
   )
))

