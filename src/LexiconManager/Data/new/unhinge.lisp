;;;;
;;;; W::unhinge
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::unhinge
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("unhinge%2:30:00"))
     (LF-PARENT ONT::unattach)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))

