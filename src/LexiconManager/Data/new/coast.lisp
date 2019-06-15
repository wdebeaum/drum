;;;;
;;;; W::COAST
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::COAST
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("coast%1:17:00"))
     (LF-PARENT ONT::shore)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::coast
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("coast%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

