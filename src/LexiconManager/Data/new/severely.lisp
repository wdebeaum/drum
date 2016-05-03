;;;;
;;;; W::severely
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	  (W::severely
	  (SENSES
	   ((LF-PARENT ONT::severity-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are severely swollen")
	    (SEM (f::gradability +) (f::orientation -) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::severity-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he spoke severely")
	    (SEM (f::gradability +) (f::orientation -) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

