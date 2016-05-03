;;;;
;;;; W::punc-minus
;;;;

(define-words :pos W::punc :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:punc)
 :words (
  (W::punc-minus
   (SENSES
    ((LF (W::MINUS))
     (syntax (w::skip +))   ;; minus is commonly a hyphen, which is not handled well in the grammar
     (non-hierarchy-lf t))
    )
   )
))

