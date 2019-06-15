;;;;
;;;; W::prowl
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::prowl
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("search-35.2") :wn ("prowl%2:38:00"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like check,search
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("prowl%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

