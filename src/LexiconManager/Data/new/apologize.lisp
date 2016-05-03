;;;;
;;;; W::apologize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::apologize
   (SENSES
    ((LF-PARENT ONT::apologize)
     (meta-data :origin calo :entry-date 20040915 :change-date 20090508 :comments calo)
     (example "he apologized to her [about it]")
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp w::to)))))
     )
    ((LF-PARENT ONT::apologize)
     (meta-data :origin calo :entry-date 20040915 :change-date 20090508 :comments calo)
     (example "he apologized")
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

