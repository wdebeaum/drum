;;;;
;;;; W::metamorphose
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::metamorphose
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("turn-26.6.1") :wn ("metamorphose%2:30:00"))
     (LF-PARENT ONT::life-transformation)
     (TEMPL AFFECTED-RESULT-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::into)))) ; like translate but pp is into, not to
     )
    )
   )
))

