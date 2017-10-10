;;;;
;;;; W::MILEAGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MILEAGE
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("mileage%1:07:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::mileage-scale)
     (templ other-reln-mass-templ)
     (EXAMPLE "determine the mileage between the two cities")
     )
    ((meta-data :origin domain-reorganization :entry-date 20170810 :change-date nil :wn ("mileage%1:24:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::distance-per-gasoline-scale)
     (EXAMPLE "this car gets good mileage")
     )
    )
   )
))

