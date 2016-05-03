;;;;
;;;; W::badly
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
;; defining these independently from the adj form to get the right handling in cardiac
 	 (W::badly
	  (SENSES
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
	    (example "his ankles are badly swollen")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL PRED-VP-TEMPL)
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
	    (example "his ankles swelled badly")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

