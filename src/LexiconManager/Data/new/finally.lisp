;;;;
;;;; W::FINALLY
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::FINALLY
   (SENSES
    ;; Myrosia 01/27/02 added as a discource adverbial: finally, ....
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::FINALLY
   (SENSES
    ((LF-PARENT ONT::temporal-location)
     (TEMPL PRED-S-VP-TEMPL)
     )
    )
   )
))

