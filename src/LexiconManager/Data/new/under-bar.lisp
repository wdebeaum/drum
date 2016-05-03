;;;;
;;;; W::under-bar
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::under-bar
   (SENSES
    ((LF (W::UNDER-BAR))
     (syntax (w::skip +))
     (non-hierarchy-lf t))
    )
   )
))

