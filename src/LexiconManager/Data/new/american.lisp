;;;;
;;;; w::american
;;;;

;(define-words :pos w::N 
; :words (
;  (w::american
;  (senses((LF-parent ONT::nationality-val) 
;	    (templ count-pred-templ)
;	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
;	    ))
;)
;))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::american
   (SENSES
    ((meta-data :origin adjective reorganization :entry-date 20170403 :change-date nil :comments nil :wn nil :comlex nil)
     (lf-parent ont::nationality-val)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::AMERICAN W::SHAD)
  (senses
	   ((LF-PARENT ONT::FRESHWATER-fish)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))


