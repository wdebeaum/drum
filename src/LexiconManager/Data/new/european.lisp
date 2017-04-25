;;;;
;;;; w::european
;;;;

#|
(define-words :pos w::N 
 :words (
  (w::european
  (senses((lf-parent ont::regional-identity-val)
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))
|#

(define-words :pos W::n
 :words (
  ((W::EUROPEAN W::TURBOT)
  (senses
	   ((LF-PARENT ONT::SALTWATER-fish)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))


(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::european
   (SENSES
    ((meta-data :origin adjective-reorganization :entry-date 20170403 :change-date nil :comments nil :wn nil)
     (lf-parent ont::regional-identity-val)
     )
    )
   )
))