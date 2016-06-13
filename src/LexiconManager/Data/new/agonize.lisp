;;;;
;;;; W::agonize
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::agonize
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("agonize%2:37:00" "agonize%2:37:01"))
     (LF-PARENT ONT::state-of-worrying)
     (TEMPL experiencer-theme-xp-templ (xp (% w::pp (w::ptype (? p w::over w::about))))) ; like mind,worry
     )
    )
   )
))

