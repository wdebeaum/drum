;;;;
;;;; w::cabinet
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::cabinet
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Office)
   ;  (LF-PARENT ont::furnishings)
     (lf-parent ont::cabinet)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  ((w::cabinet w::minister)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

