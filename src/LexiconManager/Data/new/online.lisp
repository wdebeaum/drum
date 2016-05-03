;;;;
;;;; W::online
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::online
   (SENSES
    ((LF-PARENT ONT::place)
     (example "buy it at our online store")
     (templ bare-pred-templ)
     (meta-data :origin calo-ontology :entry-date 20060417 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   (W::online
   (SENSES
    ((LF-PARENT ONT::SPATIAL-LOC)
     (TEMPL PRED-S-VP-TEMPL)
     (example "he went online")
     (meta-data :origin calo-ontology :entry-date 20060418 :change-date nil :comments nil)
     )
    )
   )
))

