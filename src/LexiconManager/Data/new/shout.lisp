;;;;
;;;; W::shout
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::shout ; like talk
   (wordfeats (W::morph (:forms (-vb) :nom W::shout)))
   (SENSES
     (;(LF-PARENT ONT::TALK)
      (LF-PARENT  ONT::manner-say)
     (example "he shouted at/to her [about it]")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp W::at w::to)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("shout%2:32:08"))
     )
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (example "he shouted about it [to/with her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("shout%2:32:08"))
     )
     ;;;; he shouted at 5 pm
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (TEMPL AGENT-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("shout%2:32:08"))
     )
    )
   )
))

