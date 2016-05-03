;;;;
;;;; W::ZIP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::ZIP W::DRIVE)
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::zip w::code)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::zip W::codes))))
   (SENSES
    ((LF-PARENT ONT::zipcode)
     (EXAMPLE "rain is forecast for this zipcode")
     (meta-data :origin task-learning :entry-date 20051109 :change-date nil :wn ("zip_code%1:10:00") :comments nil)
     )
    )
   )
))

