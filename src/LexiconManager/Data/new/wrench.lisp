;;;;
;;;; w::wrench
;;;;

(define-words :pos W::n
 :words (
  (w::wrench
  (senses
   ((LF-PARENT ONT::tool)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::wrench
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("remove-10.1") :wn ("wrench%2:35:00"))
     (LF-PARENT ONT::pull)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like eliminate
     )
    )
   )
))

