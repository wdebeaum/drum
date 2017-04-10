
(define-words :pos w::adv
 :words (
  (w::unbearably
  (senses((TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (lf-parent ont::degree-modifier-high)
	    (meta-data :origin cardiac :entry-date 20090312 :change-date nil :comments nil)
	    )
	   )
)
))

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	(W::unbearably
	  (SENSES
    ((lf-parent ont::not-tolerable-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are unbearably weak" "he is unbearably lazy")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
    ((lf-parent ont::not-tolerable-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed unbearably on the exam")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::hi))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))

