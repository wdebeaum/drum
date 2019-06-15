;;;;
;;;; W::insult
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::insult
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("insult%2:32:00"))
     (LF-PARENT ont::insult)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090520 :comments nil :vn ("amuse-31.1") :wn ("insult%2:32:00"))
     (LF-PARENT ont::evoke-offense)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

