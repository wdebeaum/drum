;;;;
;;;; w::tolerably
;;;;

(define-words :pos w::adv
 :words (
  (w::tolerably
  (senses((TEMPL ADJ-ADV-OPERATOR-TEMPL)
	    (LF-PARENT ONT::DEGREE-MODIFIER-MED)
	    (meta-data :origin cardiac :entry-date 20090312 :change-date nil :comments nil)
	    )
	   )
)
))

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	 (W::tolerably
	  (SENSES
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are tolerably swollen")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity f::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "his ankles swelled tolerably")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity f::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

