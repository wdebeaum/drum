;;;;
;;;; W::oppressively
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
         (W::oppressively
	  (SENSES
	   ((LF-PARENT ONT::severity-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are oppressively swollen")
	    (SEM (f::gradability +) (f::orientation -) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::severity-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he spoke oppressively")
	    (SEM (f::gradability +) (f::orientation -) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

