;;;;
;;;; W::than
;;;;

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::than
   (SENSES
    ((LF (W::THAN))
     (non-hierarchy-lf t))
    )
   )
  
))

(define-words :pos W::ADV :boost-word t :templ binary-constraint-s-decl-gap-templ
 :tags (:base500)
 :words (
  (W::than
   (SENSES
    ((LF (W::THAN))
     (non-hierarchy-lf t))
    )
   )
  ))

