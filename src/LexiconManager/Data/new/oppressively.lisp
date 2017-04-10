
(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
         (W::oppressively
	  (SENSES
    ((lf-parent ont::severe-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are oppressively swollen")
	    (SEM (f::gradability +) (f::orientation -) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::severe-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he spoke oppressively")
	    (SEM (f::gradability +) (f::orientation -) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

