;;;;
;;;; W::PRODUCT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::PRODUCT
    (SENSES
     ((LF-PARENT ONT::number-result)
      (meta-data :origin leam :entry-date 20070504 :change-date nil :comments nil)
      (example "the product of A and B")
      (TEMPL OTHER-RELN-TEMPL)
      )
     )
    )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::product
   (SENSES
    ((LF-PARENT ONT::product)
     (EXAMPLE "The SOFTWARE PRODUCT may contain support for programs written in Java")
     (meta-data :origin task-learning :entry-date 20050816 :change-date 20070612 :wn ("product%1:06:00" "product%1:06:01") :comments nil)
     )
    ((LF-PARENT ONT::outcome)
     (meta-data :origin caloy4 :entry-date 20070713 :change-date nil :comments plowpqs)
     )
    )
   )
))

