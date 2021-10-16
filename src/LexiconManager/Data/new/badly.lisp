;;;;
;;;; W::badly
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
;; defining these independently from the adj form to get the right handling in cardiac
 	 (W::badly
	  (SENSES
    ((lf-parent ont::bad)
	    (TEMPL ADJ-OPERATOR-TEMPL)
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity ONT::med))
	    (example "his ankles are badly swollen")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::bad)
	    (TEMPL PRED-S-OR-ADJP-TEMPL) ;(TEMPL PRED-VP-TEMPL)
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity ONT::med))
	    (example "his ankles swelled badly")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

