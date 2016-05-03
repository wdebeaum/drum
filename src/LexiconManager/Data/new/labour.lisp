;;;;
;;;; W::labour
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::labour ; would conflict with noun labor, except for UK spelling -- wdebeaum
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("cooperate-71-3"))
     (LF-PARENT ONT::working)
     (TEMPL agent-templ) ; like work
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("cooperate-71-3"))
     (LF-PARENT ONT::working)
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype (? p w::on))))) ; like work
     )
    )
   )
))

