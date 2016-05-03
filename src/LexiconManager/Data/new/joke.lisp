;;;;
;;;; W::joke
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::joke
   (wordfeats (W::morph (:forms (-vb) :nom w::joke)))
   (SENSES
    (;(LF-PARENT ONT::announce)
     (lf-parent ont::joke)
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::with)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date 20090506 :comments nil :vn ("correspond-36.1") :wn ("joke%2:32:00"))
     (example "he joked with her [about it]")
     )
    (;(LF-PARENT ONT::announce)
     (lf-parent ont::joke)
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060607 :change-date 20090506 :comments nil :vn ("correspond-36.1") :wn ("joke%2:32:00"))
     (example "he joked about it [to/with her]")
     )
    )
   )
))

