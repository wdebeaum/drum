;;;;
;;;; W::disassemble
;;;;

#||(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::disassemble
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("disassemble%2:36:00"))
     (LF-PARENT ONT::unattach)
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))||#

(define-words :pos W::v 
 :words (
  (W::disassemble
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("disassemble%2:36:00"))
     (LF-PARENT ONT::separation)
     ;;(TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype (? t w::from))))) ; like detach
      (TEMPL AGENT-affected-xp-TEMPL)
     )
    )
   )
))

