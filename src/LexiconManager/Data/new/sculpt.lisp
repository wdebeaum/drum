;;;;
;;;; W::sculpt
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::sculpt
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090504 :comments nil :vn ("build-26.1-1"))
     (LF-PARENT ONT::shape-change)
     (TEMPL agent-affected-result-templ (xp (% w::pp (w::ptype w::into)))) ; like carve
     )
    )
   )
))

