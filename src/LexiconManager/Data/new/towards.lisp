;;;;
;;;; W::TOWARDS
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::TOWARDS
   (SENSES
    ((LF-PARENT ONT::towards)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
   (W::TOwards
   (SENSES
    ((LF (W::TOwards))
     (non-hierarchy-lf t))
    )
   )
))

