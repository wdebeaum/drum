;;;;
;;;; W::disengage
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::disengage
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("remove-10.1") :wn ("disengage%2:35:00"))
     (LF-PARENT ONT::separation)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like eliminate
     )
    )
   )
))

