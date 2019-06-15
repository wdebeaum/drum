;;;;
;;;; w::swap
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::swap
	   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("exchange-13.6-1-1"))
     (LF-PARENT ONT::exchange)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% w::pp (w::ptype w::for)))) ; like substitute
     (PREFERENCE 0.96)
     )
	    ((lf-parent ont::exchange)
	     (example "swap the lamps")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	   	   
	    ))
))

