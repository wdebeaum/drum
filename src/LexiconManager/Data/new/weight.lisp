;;;;
;;;; W::WEIGHT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::WEIGHT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("weight%1:07:00"))
     (LF-PARENT ONT::WEIGHT-alt-scale)
     (example "the weight of the truck")
     (TEMPL OTHER-RELN-MASS-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("weight%1:07:00"))
     (LF-PARENT ONT::WEIGHT-alt-scale)
     (example "the weight of five pounds")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

