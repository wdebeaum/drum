;;;;
;;;; W::unleash
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::unleash
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("unleash%2:35:01"))
     (LF-PARENT ONT::unattach)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))

