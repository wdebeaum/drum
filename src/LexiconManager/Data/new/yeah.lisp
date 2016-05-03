;;;;
;;;; W::yeah
;;;;

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::yeah
   (SENSES
    ((LF (W::POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::yeah W::yeah)
   (SENSES
    ((LF (W::POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

