;;;;
;;;; W::tundra
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::tundra
   (SENSES
    ((LF-PARENT ONT::geo-formation)
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("tundra%1:17:00") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::TUNDRA W::HERB)
  (senses
	   ((LF-PARENT ONT::SPICES-HERBS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

