;;;;
;;;; W::surround
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::surround
   (SENSES
    ((LF-PARENT ONT::surround)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (example "the triangle surrounds three tomatoes")
     (meta-data :origin fruitcarts :entry-date 20050401 :change-date nil :comments nil :vn ("contiguous_location-47.8") :wn ("surround%2:33:00" "surround%2:35:00"))
     )
    ((LF-PARENT ONT::cause-surround)
     ;(TEMPL agent-affected-xp-templ (xp (% W::PP (W::ptype (? pt W::with W::in)))))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil)
     (example "The army surrounded the fort")
     )
    )
   )
))

