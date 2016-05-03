;;;;
;;;; W::SEVERAL
;;;;

(define-words :pos W::quan :boost-word t
 :words (
  (W::SEVERAL
   (wordfeats (W::status W::indefinite-plural))
   (SENSES
    ((LF W::SEVERAL)
     (non-hierarchy-lf t) (TEMPL quan-cardinality-pl-templ)
     )
    )
   )
))

