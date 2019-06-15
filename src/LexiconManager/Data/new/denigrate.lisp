;;;;
;;;; W::denigrate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::denigrate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33") :wn ("denigrate%2:32:00" "denigrate%2:32:01"))
     (LF-PARENT ONT::criticize)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::criticize)
     (example "he denigrated it")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

