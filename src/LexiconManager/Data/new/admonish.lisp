;;;;
;;;; W::admonish
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::admonish
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::criticize)
     (example "he admonished him about his bad behavior")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL) ; like warn
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::warn)
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     )
    )
   )
))

