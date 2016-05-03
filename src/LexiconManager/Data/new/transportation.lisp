;;;;
;;;; W::TRANSPORTATION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TRANSPORTATION
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::VEHICLE)
     )
    )
   )
))

(define-words :pos W::n 
 :words (
  (W::TRANSPORTATION
   (SENSES
    (
     (LF-PARENT ONT::TRANSPORT)
     (templ other-reln-affected-templ)
     (example "the transportation of oranges")
     )
    )
   )
))
