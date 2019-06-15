;;;;
;;;; W::allow
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::allow
   (SENSES
    ((LF-PARENT ONT::ALLOW)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
     (example "allow the team by the lake to go home")
     )
    ((meta-data :origin plow :entry-date 20060414 :change-date nil :comments nil :vn ("allow-64"))
     (LF-PARENT ONT::ALLOW)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "Cockroaches are not allowed")
     )

    ((LF-PARENT ONT::ALLOW)
     (TEMPL AGENT-FORMAL-XP-NP-TEMPL)
     (example "they allow smoking"
	      ))
    
     (
      (lf-parent ont::accommodate-allow) ;; 20120524 GUM change new parent
      (example "the budget allows for a thousand dollars")
      (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype W::for))))
      (meta-data :origin calo :entry-date 20040507 :change-date nil :comments y1-variations)
     )
    )
   )
))

