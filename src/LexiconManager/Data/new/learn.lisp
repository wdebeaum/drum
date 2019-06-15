;;;;
;;;; w::learn
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
	  (w::learn
	   (senses
	    ((lf-parent ont::determine)
	     (Example "I learned that voltage is a difference in charge")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype w::s-that))))
	     (meta-data :origin bee :entry-date 20040608 :change-date nil :comments portability-experiment)
	     )
	    ((lf-parent ONT::learn)
	     (Example "I learned French")
	     (TEMPL AGENT-FORMAL-XP-TEMPL)
	     ;; Put it down as education-teaching until a better classification is found
	     (meta-data :origin bee :entry-date 20040608 :change-date nil :comments portability-experiment :wn ("learn%2:31:00"))
	     )	    
	    ((lf-parent ONT::learn)
	     (Example "I learned about difference in potentials")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::pp (w::ptype w::about))))
	     ;; Put it down as education-teaching until a better classification is found
	     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-experiment  :wn ("learn%2:31:00"))
	     )	    
	    ((LF-PARENT ONT::learn)
	     (example "I want to learn how to do it")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::NP (W::sort W::wh-desc))))
	     (meta-data :origin calo :entry-date 20040622 :change-date nil :comments y2  :wn ("learn%2:31:00"))
	     )
	    ((lf-parent ont::learn)
	     (Example "I learned to sing.")
	     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL)
	     )	    
	    ))
))

