;;;;
;;;; W::ACTUALLY
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::ACTUALLY
   (SENSES
    ((LF-PARENT ONT::DEGREE-OF-BELIEF)
     (LF-FORM W::CERTAIN)
     (TEMPL PRED-VP-TEMPL)
     )
    ((LF-PARENT ONT::QUALIFICATION)
     (LF-FORM W::REAL)
     (TEMPL disc-templ)
     )
    )
   )
))

