;;;;
;;;; W::confess
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::confess
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("confess-37.10"))
     (LF-PARENT ONT::ASSERT)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype (? c w::s-that))))) ; like acknowledge
     )
    
    )
   )
))

