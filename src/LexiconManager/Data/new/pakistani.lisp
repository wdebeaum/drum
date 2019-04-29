;;;;
;;;; w::pakistani
;;;;

(define-words :pos w::N 
 :words (
  (w::pakistani
  (senses((LF-parent ONT::person-of-nationality) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::pakistani
   (SENSES
    ((meta-data :origin adjective-reorganization :entry-date 20170403 :change-date nil :comments nil :wn nil :comlex nil)
     (lf-parent ont::nationality-val)
     )
    )
   )
))
