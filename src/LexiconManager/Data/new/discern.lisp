;;;;
;;;; W::discern
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::discern
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like see
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-theme-xp-templ) ; like notice
     )
    )
   )
))

