;;;;
;;;; W::DIRECTION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DIRECTION
   (SENSES
    #|
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::LOCATION)
     (TEMPL OTHER-RELN-TEMPL)
     )
    |#
    ((LF-PARENT ONT::direction)
     (example "turn the other direction")
     (meta-data :origin fruitcarts :entry-date 20050427 :change-date nil :wn ("direction%1:15:00") :comments nil)
     )
    )
   )
))

