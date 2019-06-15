;;;;
;;;; W::confer
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::confer
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("confer%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::discuss)
     (TEMPL agent-templ) ; like argue,chat
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("confer%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::discuss)
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL) ; like argue,chat
     )
    )
   )
))

