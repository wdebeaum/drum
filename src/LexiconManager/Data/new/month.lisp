;;;;
;;;; W::MONTH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MONTH
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-unit)
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (templ time-reln-templ)
     )
    )
   )
))

