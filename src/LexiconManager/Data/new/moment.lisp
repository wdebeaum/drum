;;;;
;;;; W::MOMENT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MOMENT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("moment%1:28:01"))
     (LF-PARENT ONT::Time-unit)
     (example "a moment of silence")
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-point)
     (templ time-reln-templ)
     )
    )
   )
))

