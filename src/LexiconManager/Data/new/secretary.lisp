;;;;
;;;; W::secretary
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::secretary
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051221 :change-date nil :comments nil)
     (LF-PARENT ONT::professional)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::secretary
  (senses
	   ((LF-PARENT ONT::professional)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

