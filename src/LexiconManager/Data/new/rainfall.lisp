;;;;
;;;; W::rainfall
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::rainfall
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("rain%1:19:00"))
     ;(LF-PARENT ONT::precipitation)
     (LF-PARENT ont::precipitating)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))
