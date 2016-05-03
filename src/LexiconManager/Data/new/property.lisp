;;;;
;;;; W::property
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::property
   (SENSES
    ((LF-PARENT ont::attribute)
     (TEMPL OTHER-RELN-TEMPL)
     (EXAMPLE "a hypothetical breed property for our dog example")
     (meta-data :origin task-learning :entry-date 20050817 :change-date nil :wn ("property%1:09:00") :comments nil)
     )
    ((LF-PARENT ONT::possession)
     
     (EXAMPLE "don't steal my property")
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :wn ("property%1:21:00") :comments nil)
     )
    )
   )
))

