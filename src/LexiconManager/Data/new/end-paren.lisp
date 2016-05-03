;;;;
;;;; W::end-paren
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
    (W::end-paren
   (SENSES
    ((LF (W::end-paren))
     (non-hierarchy-lf t))
      )
   )
))

