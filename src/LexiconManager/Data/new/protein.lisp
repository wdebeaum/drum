;;;;
;;;; w::protein
;;;;

(define-words :pos W::n
 :words (
  (w::protein
  (senses
   ((LF-PARENT ONT::protein)
    (TEMPL count-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    )
   )
  )
))

(define-words :pos W::n
 :words (
  ((w::protein w::family)
   (senses
    ((LF-PARENT ONT::protein-family)
     (TEMPL count-PRED-TEMPL)
     (meta-data :origin drum :entry-date 2014 :change-date nil :comments nil)))
  )
))

