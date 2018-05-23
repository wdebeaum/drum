;;;;
;;;; W::century
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::century
   (SENSES
    ((meta-data :origin allison :entry-date 20040921 :change-date nil :wn ("century%1:28:00") :comments caloy2)
     (LF-PARENT ONT::time-unit)
     (templ unit-templ)
     )
    ((meta-data :origin plow :entry-date 20080117 :change-date nil :comments nil :wn ("month%1:28:00"))
     (LF-PARENT ONT::century)
     (templ time-reln-templ)
     )
    )
   )
))

