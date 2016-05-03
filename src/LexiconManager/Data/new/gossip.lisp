;;;;
;;;; W::gossip
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::gossip
   (wordfeats (W::morph (:forms (-vb) :past W::gossiped :ing W::gossiping)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("gossip%2:32:00" "gossip%2:32:01"))
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::schmooze-talk)
     (TEMPL agent-templ) ; like argue,chat
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("gossip%2:32:00" "gossip%2:32:01"))
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::schmooze-talk)
     (TEMPL agent-about-theme-addressee-optional-templ) ; like argue,chat
     )
    )
   )
))

