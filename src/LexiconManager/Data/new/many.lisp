;;;;
;;;; W::MANY
;;;;

(define-words :pos W::quan :boost-word t
 :words (
  (W::MANY
   (wordfeats (W::status W::indefinite-plural) (W::negatable +))
   (SENSES
    ((LF W::MANY)
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-templ)
     )
    )
   )
))

