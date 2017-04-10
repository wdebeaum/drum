
(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
 	 (W::badly
	  (SENSES
    ((lf-parent ont::bad)
	    (TEMPL ADJ-OPERATOR-TEMPL)
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
	    (example "his ankles are badly swollen")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::bad)
	    (TEMPL PRED-VP-TEMPL)
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
	    (example "his ankles swelled badly")
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

