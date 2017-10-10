;;;;
;;;; W::reservation
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
   (W::reservation
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("reservation%1:09:00") :comments meeting-understanding)
     (LF-PARENT ONT::opinion)
     (example "i recommend him with no reservations")
     )
    ((LF-PARENT ONT::reserve)
     (example "he made a reservation for 6pm")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("reservation%1:10:01" "reservation%1:09:01") :comments Office)
     )
    )
   )
))

