;;;;
;;;; W::GRANT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::GRANT
   (SENSES
    ((meta-data :origin calo :entry-date 20040406 :change-date nil :wn ("grant%1:21:00") :comments calo-y1v3)
     (example "charge it to the grant")
     (LF-PARENT ONT::account)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::grant
   (SENSES
    ((lf-parent ont::giving)
     (TEMPL AGENT-AFFECTED-TEMPL)
     (example "grant everyone access in the class")
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::giving)
     (TEMPL agent-affected-goal-optional-templ) ; like grant,offer
     )
    
    )
   )
))

