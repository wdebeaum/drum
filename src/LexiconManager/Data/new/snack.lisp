;;;;
;;;; W::snack
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
(W::snack
    (SENSES
    ((meta-data :origin cardiac :entry-date 20090406 :change-date nil :comments nil)
     (LF-PARENT ONT::consume)
     (example "he snacked [on bread]")
     (SEM (F::ASPECT F::DYNAMIC))
     (TEMPL AGENT-AFFECTED-XP-OPTIONAL-B-TEMPL  (xp (% W::pp (W::ptype W::on))))
     )
     )
   )
))

