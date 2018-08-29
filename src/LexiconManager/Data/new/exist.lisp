;;;;
;;;; w::exist
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (w::exist
	   (senses
	    ((lf-parent ont::exists)
	     (example "A solution exists")
	     (templ theme-templ )
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )
    ((LF-PARENT ONT::EXISTS)
     (TEMPL THERE-THEME-TEMPL (xp (% w::NP (w::agr (? a w::3s w::3p)))) )
     (example "There exists a solution.")
     )
	    
	    ))
))

