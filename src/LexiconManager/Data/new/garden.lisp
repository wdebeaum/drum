;;;;
;;;; w::garden
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::garden
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments Office)
     (LF-PARENT ONT::structure-external-component)
     (templ gen-part-of-reln-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::GARDEN W::VEGETABLE)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

