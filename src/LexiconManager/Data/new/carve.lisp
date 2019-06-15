;;;;
;;;; W::carve
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::carve
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("build-26.1-1"))
     (LF-PARENT ONT::shape-change) ; like build,make
     (example "he carved the wood into a chair")
     (TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL (xp (% W::PP (W::ptype W::into))))
     )
    ((LF-PARENT ONT::shape-change) ; like build,make
     (example "he carved a chair")
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
     (preference .98)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("carve-21.2-2"))
     ;;(LF-PARENT ONT::cut) ; like chop
     (lf-parent ont::carve-cut) ;; 20120524 GUM change new parent
     (example "he carved the turkey")
     )
    )
   )
))

