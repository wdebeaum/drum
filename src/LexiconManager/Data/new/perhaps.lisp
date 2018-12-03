;;;;
;;;; W::perhaps
;;;;

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::perhaps
   (SENSES
    ((LF (ONT::UNSURE-POS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  ((W::perhaps w::not)
   (SENSES
    ((LF (ONT::UNSURE-NEG))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_RESPONSE))
     )
    )
   )
))
