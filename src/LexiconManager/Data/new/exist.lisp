;;;;
;;;; w::exist
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::exist
	   (senses
	    ((lf-parent ont::exists)
	     (example "A solution exists")
	     (templ neutral-templ )
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )
    ((LF-PARENT ONT::EXISTS)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL (xp (% w::NP (w::agr (? a w::3s w::3p)))) )
     (example "There exists a solution.")
     )
	    
	    ))
))

