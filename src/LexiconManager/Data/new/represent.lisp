;;;;
;;;; w::represent
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::represent
	   (senses
	    ((lf-parent ont::categorization)
	     (templ agent-neutral-xp-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment :vn ("characterize-29.2-1-1"))
	     )	   	   	    
	    ((lf-parent ont::encodes-message)
	     (example "this diagram represents circuit 5")
	     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
	     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-followup)
	     )
	    ((lf-parent ont::represent)
	     (example "he represented him in court")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     )
	    ))
))

