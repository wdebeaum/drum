;;;;
;;;; W::LINE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::LINE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::linear-grouping)
     (example "a line of blocks")
     (templ other-reln-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::grouping)
     (example "a product line")
     (templ count-pred-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("line%1:10:01"))
     (LF-PARENT ONT::graphic-symbol)
     (example "the red line on the map")
     )
    )
   )
))

