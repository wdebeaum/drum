;;;;
;;;; W::salute
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::salute
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("salute%2:34:00"))
     (LF-PARENT ONT::congratulate)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::congratulate)
     (example "he saluted the flag")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

