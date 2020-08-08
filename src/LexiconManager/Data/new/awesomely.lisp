;;;;
;;;; w::awesomely
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	(W::awesomely
	  (SENSES
    ((lf-parent ont::degree-modifier-high)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are awesomely strong" "he is awesomely healthy")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::great-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed awesomely on the exam")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

