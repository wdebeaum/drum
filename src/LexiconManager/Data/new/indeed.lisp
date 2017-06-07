;;;;
;;;; W::INDEED
;;;;

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  (W::INDEED
   (SENSES
    ((LF-PARENT ONT::DEGREE-OF-BELIEF)
     (LF-FORM W::INDEED)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::INDEED)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::INDEED
   (SENSES
    ((LF (ONT::POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

