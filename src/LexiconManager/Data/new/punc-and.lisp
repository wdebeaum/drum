;;;;
;;;; W::punc-and
;;;;

(define-words :pos W::conj :boost-word t 
 :words (
  (W::punc-and
   (wordfeats (W::conj +) (W::seq +))
   (SENSES
    ((LF (ONT::AND))
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-ANY-TEMPL)
     (syntax (w::status (? s ont::definite-plural ont::indefinite-plural)))
     )
    )
   )
))

