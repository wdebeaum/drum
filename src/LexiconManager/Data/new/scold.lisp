;;;;
;;;; W::scold
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::scold
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("judgement-33") :wn ("scold%2:32:00"))
     (LF-PARENT ont::reprimand)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090507 :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::reprimand)
     (example "he scolded it")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

