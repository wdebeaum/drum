;;;;
;;;; W::punc-semicolon
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::punc-semicolon
   (SENSES
    ((LF (W::SEMICOLON))
     (syntax (w::skip +))
     (non-hierarchy-lf t))
    )
   )
))

