;;;;
;;;; W::INTERVAL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::INTERVAL
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("interval%1:28:00"))
     (LF-PARENT ONT::Time-interval)
     (templ time-reln-templ)
     )
    ((meta-data :wn ("musical_interval%1:10:00"))
     (LF-PARENT ONT::PITCH-INTERVAL)
     ;roughly the difference in pitch between two notes
     ;not sure about the template
     (EXAMPLE "Major third is an interval.")
     )
    ((meta-data :origin fruit-carts :entry-date 20050427 :change-date nil :wn ("space%1:25:00") :comments fruitcart-11-4)
     (example "distribute the items at even intervals")
     (LF-PARENT ONT::distance-scale)
;     (LF-PARENT ONT::interval)
     (preference .98)
     )
    )
   )
))

