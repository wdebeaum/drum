;;;;
;;;; w::contest
;;;;

(define-words :pos W::n
 :words (
  (w::contest
  (senses;;;;;
   ((LF-PARENT ONT::competition)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (w::contest
	   (senses
	    (;;(LF-parent ont::contest)
	     (lf-parent ont::contest-deny-oppose-protest) ;; 20120523 GUM change new parent
	     (Example "He contests the findings")
	     (TEMPL AGENT-THEME-XP-TEMPL)
	     (meta-data :origin task-learning :entry-date 20050825 :change-date 20090508 :comments nil) 
	     )	    
	    ))
))

