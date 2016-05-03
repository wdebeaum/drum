;;;;
;;;; W::dislodge
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::dislodge
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090529 :comments nil :vn ("remove-10.1") :wn ("dislodge%2:35:00" "dislodge%2:35:01"))
     (LF-PARENT ONT::separation)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like eliminate
     )
    )
   )
))

