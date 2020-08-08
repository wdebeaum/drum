;;;;
;;;; w::tolerably
;;;; 

(define-words :pos w::adv
 :words (
  (w::tolerably
  (senses((TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (lf-parent ont::degree-modifier-med)
	    (meta-data :origin cardiac :entry-date 20090312 :change-date nil :comments nil)
	    )
	   )
)
))

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	 (W::tolerably
	  (SENSES
    ((lf-parent ont::tolerable-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are tolerably swollen")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::tolerable-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "his ankles swelled tolerably")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity ONT::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

