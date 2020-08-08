;;;;
;;;; W::nicely
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
         (W::nicely
	  (SENSES
    ((lf-parent ont::good)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are nicely recovered" "he is nicely situated")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::good)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed nicely on the exam")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

