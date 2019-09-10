;;;;
;;;; W::shout
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::shout ; like talk
   (wordfeats (W::morph (:forms (-vb) :nom W::shout)))
   (SENSES
     (;(LF-PARENT ONT::TALK)
      (LF-PARENT  ONT::manner-say)
     (example "he shouted at/to her [about it]")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp W::at w::to)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("shout%2:32:08"))
     )
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (example "he shouted about it [to/with her]")
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("shout%2:32:08"))
     )
     ;;;; he shouted at 5 pm
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (TEMPL AGENT-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("shout%2:32:08"))
     )

    ((LF-PARENT  ONT::manner-say)
     (TEMPL agent-formal-for-to-subjcontrol-templ)
     )

    )
   )
))

