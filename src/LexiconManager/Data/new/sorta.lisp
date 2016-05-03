;;;;
;;;; w::sorta
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (w::sorta
   (SENSES
    ((LF-PARENT ONT::DEGREE-OF-BELIEF)
     (LF-FORM W::SORT-OF)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::degree-MODIFIER-MED)
     (LF-FORM W::SORT-OF)
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    ((LF-PARENT ONT::DEGREE-MODIFIER-MED)
     (LF-FORM W::SORT-OF)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

