;;;;
;;;; W::MINUTE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::MINUTE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("minute%1:28:00"))
     (LF-PARENT ONT::minute-duration)
     (example "five minutes of silence")
     (templ unit-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("minute%1:28:00"))
     (LF-PARENT ONT::angle-unit)
     (example "nine degrees five minutes")
     (templ attribute-unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-defined-by-duration)
     (templ time-reln-templ)
     )
    )
   )
))

