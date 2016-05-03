;;;;
;;;; W::punc-plus
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::punc-plus
   (SENSES
    ((LF (W::PLUS))
     (non-hierarchy-lf t))
    )
   )
))

