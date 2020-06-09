;;;;
;;;; W::MANY
;;;;

(define-words :pos W::quan :boost-word t
 :words (
  (W::MANY
   (wordfeats (W::status ont::indefinite-plural) (W::negatable +))
   (SENSES
    ((LF ONT::MANY)
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-convertable-templ)
     )
    )
   )
))

