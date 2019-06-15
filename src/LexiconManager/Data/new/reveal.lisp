;;;;
;;;; W::reveal
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::reveal
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("characterize-29.2-1-2"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::tell)
     (TEMPL AGENT-FORMAL-AGENT1-OPTIONAL-TEMPL) ; like report
     (PREFERENCE 0.97)
     )
    ((meta-data :origin cernl :entry-date 20110802 :change-date nil :comments nil)
     (LF-PARENT ONT::reveal)
     (example "the test revealed the problem")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (preference .98) ; prefer agentive
     )
    ((meta-data :origin cernl :entry-date 20110802 :change-date nil :comments nil) 
     (LF-PARENT ONT::reveal)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% w::cp (w::ctype w::s-that)))) 
     (PREFERENCE 0.98) ; prefer agentive
     )
    )
   )
))

