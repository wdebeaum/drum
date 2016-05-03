;;;;
;;;; W::insufferably
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	  (W::insufferably
	  (SENSES
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are insufferably swollen" "he is insufferably ill")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed insufferably on the exam")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

