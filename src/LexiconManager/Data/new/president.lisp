;;;;
;;;; W::president
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::president
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments nil)
     (LF-PARENT ONT::politics-professional) ;professional)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  (W::president
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

