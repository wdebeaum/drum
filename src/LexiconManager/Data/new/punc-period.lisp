;;;;
;;;; W::punc-period
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-period
   (SENSES
    ((LF (W::PERIOD))
     (non-hierarchy-lf t)(SYNTAX (W::punctype (? x W::decl W::decimalpoint)))
     )
    )
   )
))

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  ((W::punc-period W::punc-period w::punc-period)
   (SENSES
    ((LF (W::ELLIPSES))
     (non-hierarchy-lf t)(SYNTAX (W::punctype (? x W::decl W::decimalpoint)))
     )
    )
   )
))

