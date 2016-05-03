;;;;
;;;; W::INITIALLY
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::INITIALLY
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::INITIALLY
   (SENSES
    ((LF-PARENT ONT::temporal-location)
     (TEMPL PRED-VP-TEMPL)
     )
    )
   )
))

