;;;;
;;;; W::WAY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::WAY
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::method)
     (templ other-reln-templ)
     (example "this is the way to paint a house")
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("way%1:06:00"))
     (LF-PARENT ONT::ROUTE)
     (example "do you know the way to calderon")
     )
    ((LF-PARENT ONT::direction)
     (example "orient the camera this way")
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date nil :wn ("way%1:15:00") :comments Orient)
     )
     ((LF-PARENT ONT::distance)
     (example "it is a long way to the next oasis")
     (meta-data :origin navigation :entry-date 20080903 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::WAY w::POINT)
   (SENSES
    ((meta-data :origin lou2 :entry-date 20061108 :change-date nil :comments demo)
     (LF-PARENT ONT::LOCATION)
     )
    )
   )
))

