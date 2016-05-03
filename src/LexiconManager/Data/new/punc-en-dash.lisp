;;;;
;;;; W::punc-en-dash
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-en-dash
   (SENSES
    ((LF (W::EN-DASH))
     (syntax (w::skip +))
     (non-hierarchy-lf t))
    )
   )
))

