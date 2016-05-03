;;;;
;;;; W::player
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::player
   (SENSES
    ((meta-data :origin calo :entry-date 20041201 :change-date nil :comments caloy2)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::player
  (senses
   ((LF-PARENT ONT::athlete)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

