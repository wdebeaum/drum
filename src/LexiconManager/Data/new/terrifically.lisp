;;;;
;;;; W::terrifically
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	  (W::terrifically
	  (SENSES
    ((lf-parent ont::awful-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are terrifically swollen" "he is terrifically ill")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::awful-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed terrifically on the exam")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

