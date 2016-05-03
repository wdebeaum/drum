;;;;
;;;; W::snack
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::snack
    (SENSES
    ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he snacked [on bread]")
     (SEM (F::ASPECT F::DYNAMIC))
     (templ agent-affected-optional-templ  (xp (% W::pp (W::ptype W::on))))
     )
     )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
     (W::snack
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("nonverbal_expression-40.2"))
     (LF-PARENT ONT::consume)
     (templ agent-affected-optional-templ  (xp (% W::pp (W::ptype W::on)))) ; like dine
     )
    )
   )
))

