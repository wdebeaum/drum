;;;;
;;;; W::envelop
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::envelop
   (SENSES
    ((LF-PARENT ONT::cover)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype W::in))))
     (example "They had been packed up in a box, and enveloped in cotton")
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::cover)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     (example "Fog enveloped the house")
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil :wn ("envelop%2:35:00"))
     )
    )
   )
))

