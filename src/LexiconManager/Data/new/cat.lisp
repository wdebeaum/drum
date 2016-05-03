;;;;
;;;; w::cat
;;;;

(define-words :pos W::n
 :words (
  ((w::cat w::scan)
  (senses
   ((LF-PARENT ONT::medical-diagnostic)
    (meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
    (TEMPL COUNT-PRED-TEMPL)
    )
   )
)
))

(define-words :pos w::N 
 :tags (:base500)
 :words (
  (w::cat
  (senses((LF-parent ONT::nonhuman-animal) 
	    (templ count-pred-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

