;;;;
;;;; W::lightning
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::lightning 
   (SENSES
    ((LF-PARENT ONT::ATMOSPHERIC-PHENOMENON)
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::lightning w::bolt)
   (SENSES
    (;(LF-PARENT ONT::natural-object)
     (LF-PARENT ONT::ATMOSPHERIC-PHENOMENON)
     (EXAMPLE "click the lightning bolt icon")
     (meta-data :origin task-learning :entry-date 20050830 :change-date nil :comments nil)
     )
    )
   )
))


