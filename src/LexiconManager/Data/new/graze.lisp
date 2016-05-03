;;;;
;;;; W::graze
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   (W::graze
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2"))
     (LF-PARENT ONT::consume)
     (templ agent-affected-optional-templ  (xp (% W::pp (W::ptype W::on)))) ; like dine
     )
    )
   )
))

