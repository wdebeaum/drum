;;;;
;;;; W::k
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::k w::m)
   (SENSES
    (;(LF-PARENT ONT::length-unit)
     (LF-PARENT ONT::km)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::K
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::NUMBER-UNIT :boost-word t :templ NUMBER-UNIT-TEMPL
 :words (
  (W::K
   (SENSES
    ( (meta-data :origin calo :entry-date 20040414 :change-date nil :comments y1v7)
     (LF-PARENT ONT::NUMBER-UNIT)
     (SYNTAX (W::VAL 1000))
     )
    )
   )
))

