;;;;
;;;; w::disagree
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (w::disagree
    (wordfeats (W::morph (:forms (-vb) :nom w::disagreement)))
;;	   (wordfeats (W::morph (:forms (-vb) :past W::disagreed :ing W::disagreeing :pastpart W::disagreed)))
	   (senses
	    ((LF-parent ont::contest)
	     (Example "He disagreed [with John]")
	     (TEMPL AGENT-ADDRESSEE-OPTIONAL-TEMPL (xp (% w::PP (w::ptype w::with))))
	     (meta-data :origin calo :entry-date 20050509 :change-date 20090508 :comments projector-purchasing) 
	     )
	    ((LF-parent ont::contest)
	     (Example "He disagreed with the findings")
	     (TEMPL AGENT-THEME-XP-TEMPL)
	     (meta-data :origin calo :entry-date 20050509 :change-date 20090508 :comments projector-purchasing) 
	     )	

	    ((LF-parent ont::refute)
	     (Example "The results disagreed with the theory")
	     (TEMPL neutral-formal-as-comp-templ (xp (% w::PP (w::ptype w::with))))
	     )	    
    
	    ))
))

