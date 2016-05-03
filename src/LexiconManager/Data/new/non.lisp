;;;;
;;;; w::non
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
	  ((w::non w::zero)
	   (senses
	    ((lf-parent ont::number-related-property-val)
	     (meta-data :origin beetle :entry-date 20080221 :change-date 20090915 :comments pilot1)
	     (example "non-zero value")
	     )
	    ))	  
))

(define-words :pos w::adv
 :words (
  (w::non-
  (senses
   ((lf-parent ont::NEG)
    (example "nonword")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::non-
   (SENSES
    (
     (LF-PARENT ONT::NEG)
     (example "nonpayment; nonanswer")
     (TEMPL central-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::non-
  (senses
   ((lf-parent ont::NEG)
    (example "nonspecific; nonalcoholic; noncompetitive inhibitor")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
