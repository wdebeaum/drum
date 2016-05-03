;;;;
;;;; W::punc-times
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   (W::punc-times
   (SENSES
    ((LF (W::MULTIPLY))
     (non-hierarchy-lf t))
    )
   )
))

