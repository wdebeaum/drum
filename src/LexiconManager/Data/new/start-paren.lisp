;;;;
;;;; W::start-paren
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   (W::start-paren
   (SENSES
    ((LF (W::start-paren))
     (non-hierarchy-lf t))
      )
   )
))

