;;;;
;;;; W::slash
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ;; for fractions
  (W::slash
   (SENSES
    ((LF (W::SLASH))
     (non-hierarchy-lf t)
     )
    )
   )
))

