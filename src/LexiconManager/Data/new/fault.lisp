;;;;
;;;; W::fault
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::fault
   (wordfeats (W::morph (:forms (-vb) :nom w::fault)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("fault%2:32:00"))
     (LF-PARENT ONT::blame)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::blame)
     (example "he faulted it")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

