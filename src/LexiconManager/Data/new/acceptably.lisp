;;;;
;;;; W::acceptably
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	(W::acceptably
	  (SENSES
    ((lf-parent ont::good)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are acceptably strong" "he is acceptably healthy")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::good)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed acceptably on the exam")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

