;;;;
;;;; W::billion
;;;;

(define-words :pos W::NUMBER-UNIT :boost-word t :templ NUMBER-UNIT-TEMPL
 :words (
  (W::billion
   (SENSES
    ((LF-PARENT ONT::NUMBER-UNIT)
     (SYNTAX (W::VAL 100000000))
     )
    )
   )
))

