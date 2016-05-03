;;;;
;;;; w::quarter
;;;;

(define-words :pos W::n
 :words (
  ((w::quarter w::pounder)
  (senses
	   ((LF-PARENT ONT::FAST-FOOD)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::quarter
   (SENSES
    ((LF (W::NTH 4))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype w::fraction))
     )
    )
   )
))

