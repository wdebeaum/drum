;;;;
;;;; W::PROBABLY
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::PROBABLY
   (SENSES
    ((LF-PARENT ONT::likelihood)
     (TEMPL DISC-TEMPL)
     (preference .98)
     )
    ((LF-PARENT ONT::likelihood)
     (example "he probably left")
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))

