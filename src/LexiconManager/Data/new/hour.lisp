;;;;
;;;; W::HOUR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::HOUR
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("hour%1:28:00"))
     (LF-PARENT ONT::time-unit)
     (example "5 hours") ;; quantity terms
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (example "hour of the day")
     (templ time-reln-templ)
     )
    )
   )
))

