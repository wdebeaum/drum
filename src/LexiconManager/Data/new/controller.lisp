;;;;
;;;; W::controller
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::controller
   (SENSES
    ((meta-data :origin allison :entry-date 20041027 :change-date nil :wn ("controller%1:06:00") :comments caloy2)
     (LF-PARENT ONT::DEVICE)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::controller
  (senses
   ((LF-PARENT ont::control-manage)
    (meta-data :origin asma :entry-date 20111006 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    (example "controller medicine")
    )
   )
)
))

