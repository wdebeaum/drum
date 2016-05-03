;;;;
;;;; W::cm
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::cm ;; alternate plural
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::length-unit)
     (LF-FORM W::centimeter)
     (TEMPL ATTRIBUTE-UNIT-PLURAL-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("cm%1:23:00") :comments nil)
     )
    )
   )
))

