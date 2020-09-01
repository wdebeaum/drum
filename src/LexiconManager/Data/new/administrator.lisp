;;;;
;;;; W::administrator
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::administrator
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments nil)
     (LF-PARENT ONT::manager) ;professional)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::administrator
   (SENSES
    ((LF-PARENT ONT::manager) ;professional)
     (EXAMPLE "the network administrator has the information you need")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :comments nil)
     )
    )
   )
))

