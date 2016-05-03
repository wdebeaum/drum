;;;;
;;;; w::intolerably
;;;;

(define-words :pos w::adv
 :words (
  (w::intolerably
  (senses((TEMPL ADJ-ADV-OPERATOR-TEMPL)
	    (LF-PARENT ONT::DEGREE-MODIFIER-HIGH)
	    (meta-data :origin cardiac :entry-date 20090312 :change-date nil :comments nil)
	    )
	   )
)
))

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	 (W::intolerably
	  (SENSES
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are badly swollen")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he ran badly")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

