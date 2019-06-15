;;;;
;;;; W::warrant
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (W::warrant
	   (SENSES
	    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("declare-29.4-2"))
	     (LF-PARENT ONT::believe)
	     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like think
	     )
	    )
	   )
))

