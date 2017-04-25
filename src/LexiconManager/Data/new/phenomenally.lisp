;;;;
;;;; w::phenomenally
;;;; 

(define-words :pos w::adv
 :words (
  (w::phenomenally
  (senses((TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (lf-parent ont::degree-modifier-high)
            (meta-data :origin adjective-reorganization :entry-date 20170413 :change-date nil :comments nil)
            )
           )
)
))

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
       (W::phenomenally
	  (SENSES
    ;((lf-parent ont::great-val)
	;    (TEMPL ADJ-OPERATOR-TEMPL)	    
	;    (example "his ankles are phenomenally swollen" "he is phenomenally ill")
	;    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity f::hi))
	;    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	;    )
    ((lf-parent ont::great-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed phenomenally on the exam")
	    (SEM (f::gradability +) (f::orientation f::pos) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

