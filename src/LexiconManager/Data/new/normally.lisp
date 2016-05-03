;;;;
;;;; W::NORMALLY
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::NORMALLY
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL DISC-TEMPL)
     )
    ((LF-PARENT ONT::frequency)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::NORMALLY
   (SENSES
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL PRED-VP-TEMPL)
     )
    ((LF-PARENT ONT::FREQUENCY)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

