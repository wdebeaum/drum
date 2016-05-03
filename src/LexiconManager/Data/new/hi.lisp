;;;;
;;;; W::hi
;;;;

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:greet)
 :words (
  (W::hi
   (SENSES
    ((LF (W::HELLO))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GREET))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:greet)
 :words (
  ((W::hi W::there)
   (SENSES
    ((LF (W::HELLO))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GREET))
     )
    )
   )
))

