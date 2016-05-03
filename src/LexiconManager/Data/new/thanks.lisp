;;;;
;;;; W::thanks
;;;;

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::thanks
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::thanks W::a W::lot)
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::thanks W::alot)
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::thanks W::very W::much)
   (SENSES
    ((LF (W::THANKS))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_THANK))
     )
    )
   )
))

