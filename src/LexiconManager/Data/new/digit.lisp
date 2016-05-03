;;;;
;;;; W::digit
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::digit
   (SENSES
    ((LF-PARENT ONT::number-object)
     (meta-data :origin plot :entry-date 20080505 :change-date nil :comments nil)
     (example "the last 4 digits of the ssn")
     (templ other-reln-templ)
     )
    )
   )
))

