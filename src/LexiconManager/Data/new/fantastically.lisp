;;;;
;;;; W::fantastically
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	  (W::fantastically
	  (SENSES
    ((lf-parent ont::great-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are fantastically swollen" "he is fantastically ill")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::great-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed fantastically on the exam")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

