;;;;
;;;; W::rifle
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::rifle
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("cheat-10.6") :wn ("rifle%2:40:00"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype w::through)))) ; like rid but different template and lf
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("search-35.2") :wn ("rifle%2:35:13"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL agent-theme-xp-templ) ; like check,search
     )
    )
   )
))

