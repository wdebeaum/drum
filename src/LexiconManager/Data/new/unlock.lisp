;;;;
;;;; W::unlock
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::unlock
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("unlock%2:30:00" "unlock%2:35:00"))
     (LF-PARENT ONT::unattach)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-A-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))

