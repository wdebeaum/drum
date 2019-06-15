;;;;
;;;; W::realise
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  ;; a copy of "realize"
	  (W::realise
	   (SENSES
	    ((LF-PARENT ONT::come-to-understand)
	     (lf-form w::realize)
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :comments lam-initial)
	     (example "I realized that the origional route is no longer available.")
	     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
	     )
	    ((LF-PARENT ONT::come-to-understand) (lf-form w::realize)
	     (meta-data :origin lam :entry-date 20050421 :change-date nil :comments lam-initial)
	     (example "I realize the risks")
	     (TEMPL AGENT-FORMAL-XP-TEMPL)
	     )
	    ))
))

