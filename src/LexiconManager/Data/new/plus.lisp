;;;;
;;;; W::PLUS
;;;;

(define-words :pos W::conj :boost-word t
 :words (
;   )
  (W::PLUS
   (wordfeats (W::conj +))
   (SENSES
    ((LF ONT::PLUS)
     (non-hierarchy-lf t)(TEMPL SUBCAT-ANY-TEMPL)
     )
    ((LF ONT::AND)
     (non-hierarchy-lf t)(TEMPL SUBCAT-ANY-TEMPL)
     (SYNTAX (W::seq +) (W::status (? s ont::definite-plural ont::indefinite-plural)))
     )
    )
   )
))

