;;;;
;;;; W::discern
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::discern
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like see
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like notice
     )
    )
   )
))

