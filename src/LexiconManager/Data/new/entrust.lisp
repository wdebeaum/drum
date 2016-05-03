;;;;
;;;; W::entrust
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::entrust
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::giving)
     (TEMPL agent-affected-goal-templ) ; like supply, but doesn't have ditransitive variant
     )
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::giving)
     (example "entrust him with the problem")
     (TEMPL agent-recipient-affected-templ (xp (% w::pp (w::ptype w::with))))
     )
    )
   )
))

