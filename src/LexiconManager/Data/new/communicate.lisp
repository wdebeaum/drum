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
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::to w::with)))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090505 :comments nil :vn ("battle-36.4-1"))
     (LF-PARENT ONT::communication)
     (example "he talked about it [to/with her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::communication)
     (example "communicate it to him")
     (TEMPL agent-theme-to-addressee-optional-templ)
     )
    ))   
))

