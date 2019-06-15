;;;;
;;;; W::entrust
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::entrust
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::giving)
     (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL) ; like supply, but doesn't have ditransitive variant
     )
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::giving)
     (example "entrust him with the problem")
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-NP-TEMPL (xp (% w::pp (w::ptype w::with))))
     )
    )
   )
))

