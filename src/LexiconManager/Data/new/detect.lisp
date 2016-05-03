;;;;
;;;; W::detect
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::detect
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::becoming-aware)
     (example "detect whether there is a problem")
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like see
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"))
     (LF-PARENT ONT::becoming-aware)
     (example "detect the problem")
     (TEMPL agent-theme-xp-templ) ; like notice
     )
    )
   )
))

