;;;;
;;;; W::hello
;;;;

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:greet)
 :words (
  (W::hello
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
  ((W::hello W::there)
   (SENSES
    ((LF (W::HELLO))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GREET))
     )
    )
   )
))

