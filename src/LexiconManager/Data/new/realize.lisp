;;;;
;;;; W::realize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	 (W::realize
	  (wordfeats (W::morph (:forms (-vb) :nom w::realization)))
	  (SENSES
    ;;;; I realized that the origional route is no longer available.
	   ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("conjecture-29.5-1"))
	    (LF-PARENT ONT::come-to-understand)
	    (TEMPL AGENT-FORMAL-XP-CP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
	    )
    ;;;; I realize the risk
	   ((LF-PARENT ONT::come-to-understand)
	    (TEMPL AGENT-FORMAL-XP-TEMPL)
	    )
	   )
	  )
	 ))

