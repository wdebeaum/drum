;;;;
;;;; W::communicate
;;;;

(define-words :pos W::v 
 :words (
 (W::communicate
   (wordfeats (W::morph (:forms (-vb) :nom w::communication)))
   (SENSES
    ((LF-PARENT ONT::communication)
     (example "he communicated with her [about it]")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::to w::with)))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090505 :comments nil :vn ("battle-36.4-1"))
     (LF-PARENT ONT::communication)
     (example "he talked about it [to/with her]")
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::communication)
     (example "communicate it to him")
     ;(TEMPL agent-theme-to-addressee-optional-templ)
     (TEMPL AGENT-NEUTRAL-AGENT1-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::communication)
     (example "He communicated that the cat did not eat the finch")
     (TEMPL AGENT-AFFECTED-FORMAL-2-XP-OPTIONAL-TEMPL (xp (% W::PP (w::ptype (? ptp w::to)))))
     )
    ))   
))

