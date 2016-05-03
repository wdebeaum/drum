;;;;
;;;; W::BUS
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BUS
;;   (wordfeats (W::MORPH (:forms (-S-3P -load))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("bus%1:06:00"))
     (LF-PARENT ONT::land-vehicle)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::BUS W::STATION)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::bus W::stations))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("bus_station%1:06:00"))
     (LF-PARENT ONT::transportation-facility)
     (LF-FORM W::BUS-STATION)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::bus
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("drive-11.5-1"))
     (LF-PARENT ONT::transport)
 ; like shuttle
     )
        )
   )
))

