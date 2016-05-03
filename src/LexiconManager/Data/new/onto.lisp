;;;;
;;;; W::ONTO
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::ONTO
   (SENSES
    ((LF-PARENT ONT::goal-as-on)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::ONTO
   (SENSES
    ((LF (W::ONTO))
     (non-hierarchy-lf t))
    )
   )
))

