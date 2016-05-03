;;;;
;;;; W::province
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::province
   (SENSES
    ((LF-PARENT ONT::district)
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :wn ("province%1:15:00") :comments nil)
     (example "can you name all the canadian provinces")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::province
   (SENSES
    ((LF-PARENT ONT::district)
     (EXAMPLE "this EULA is governed by the laws in force in the Province of Ontario, Canada")
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :wn ("province%1:15:00") :comments nil)
     )
    )
   )
))

