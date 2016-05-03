;;;;
;;;; W::BIT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BIT
   (SENSES
    ;; this sense is now a quantifier
;;    ((LF-PARENT ONT::QUANTITY)
;;     (example "a bit of sand")
;;     (TEMPL indef-classifier-templ)
;;     )
    ((LF-PARENT ONT::memory-unit)
     (TEMPL SUBSTANCE-UNIT-lf-of-3s-TEMPL)
     (example "64 bit memory architecture")
     (meta-data :origin plow :entry-date 20060615 :change-date nil :wn ("bit%1:23:00") :comment pq)
     )
    )
   )
))

