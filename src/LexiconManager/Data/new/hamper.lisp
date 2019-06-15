;;;;
;;;; w::hamper
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::hamper
	   (senses
	    ((lf-parent ont::hindering)
	     (example "the conflict hampers resolution")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )
	    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
	     (LF-PARENT ONT::hindering)
	     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
	     (example "It hampers him from doing something")
	     )
	    ))	 
))

