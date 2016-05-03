;;;;
;;;; W::envelop
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::envelop
   (SENSES
    ((LF-PARENT ONT::cover)
     (TEMPL agent-affected2-templ (xp (% W::PP (W::ptype W::in))))
     (example "They had been packed up in a box, and enveloped in cotton")
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::cover)
     (TEMPL neutral-neutral-templ)
     (example "Fog enveloped the house")
     (meta-data :origin trips :entry-date 20090911 :change-date nil :comments nil :wn ("envelop%2:35:00"))
     )
    )
   )
))

