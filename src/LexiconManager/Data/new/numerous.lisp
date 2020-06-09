;;;;
;;;; W::numerous
;;;;

(define-words :pos W::quan :boost-word t
 :words (
  (W::numerous
   (wordfeats (W::status ont::indefinite-plural))
   (SENSES
    ((LF ONT::SEVERAL)
     (non-hierarchy-lf t) (TEMPL quan-cardinality-pl-convertable-templ)
     (meta-data :origin step :entry-date 20080703 :change-date nil :comments nil)
     )
    )
   )
))

