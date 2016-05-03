;;;;
;;;; w::learn
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
	  (w::learn
	   (senses
	    ((lf-parent ont::determine)
	     (Example "I learned that voltage is a difference in charge")
	     (templ agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-that))))
	     (meta-data :origin bee :entry-date 20040608 :change-date nil :comments portability-experiment)
	     )
	    ((lf-parent ONT::learn)
	     (Example "I learned French")
	     (templ agent-theme-xp-templ)
	     ;; Put it down as education-teaching until a better classification is found
	     (meta-data :origin bee :entry-date 20040608 :change-date nil :comments portability-experiment :wn ("learn%2:31:00"))
	     )	    
	    ((lf-parent ONT::learn)
	     (Example "I learned about difference in potentials")
	     (templ agent-theme-xp-templ (xp (% w::pp (w::ptype w::about))))
	     ;; Put it down as education-teaching until a better classification is found
	     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-experiment  :wn ("learn%2:31:00"))
	     )	    
	    ((LF-PARENT ONT::learn)
	     (example "I want to learn how to do it")
	     (TEMPL agent-theme-xp-templ (xp (% W::NP (W::sort W::wh-desc))))
	     (meta-data :origin calo :entry-date 20040622 :change-date nil :comments y2  :wn ("learn%2:31:00"))
	     )
	    ))
))

