;;;;
;;;; W::FLIGHT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FLIGHT
   (SENSES
    ((LF-PARENT ONT::flight)
     (example "show me all flights before five pm")
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :wn ("flight%1:04:02") :comments nil)
     )
    ((LF-PARENT ONT::group-object)
     (example "a flight of stairs")
     (meta-data :origin asma :entry-date 20111005)
     (templ other-reln-templ)
     )
    ((LF-PARENT ONT::air-vehicle)
     (example "the flight departed")
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :comments nil)
     (preference .96)
     )
    )
   )
))

