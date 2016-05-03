;;;;
;;;; W::PLUS
;;;;

(define-words :pos W::conj :boost-word t
 :words (
;   )
  (W::PLUS
   (wordfeats (W::conj +))
   (SENSES
    ((LF W::PLUS)
     (non-hierarchy-lf t)(TEMPL SUBCAT-ANY-TEMPL)
     )
    ((LF W::AND)
     (non-hierarchy-lf t)(TEMPL SUBCAT-ANY-TEMPL)
     (SYNTAX (W::seq +) (W::status W::definite-plural))
     )
    )
   )
))

