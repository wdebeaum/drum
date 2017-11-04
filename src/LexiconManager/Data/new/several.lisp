;;;;
;;;; W::SEVERAL
;;;;

(define-words :pos W::quan :boost-word t
 :words (
  (W::SEVERAL
   (wordfeats (W::status ont::indefinite-plural))
   (SENSES
    ((LF ONT::SEVERAL)
     (non-hierarchy-lf t) (TEMPL quan-cardinality-pl-templ)
     )
    )
   )
))

