;;;;
;;;; W::commend
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::commend
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("commend%2:32:00" "commend%2:32:02"))
     (LF-PARENT ONT::praise)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
     ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::praise)
     (example "he commended the proposal")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

