;;;;
;;;; W::converse
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::converse
   (wordfeats (W::morph (:forms (-vb) :nom w::conversation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("converse%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::talk)
     (TEMPL agent-templ) ; like argue,chat
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("chit_chat-37.6") :wn ("converse%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::talk)
     (TEMPL agent-about-theme-addressee-optional-templ) ; like argue,chat
     )
    )
   )
))

