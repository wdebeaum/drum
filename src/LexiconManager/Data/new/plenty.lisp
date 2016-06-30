;;;;
;;;; W::PLENTY
;;;;

(define-words :pos W::quan :boost-word t
 :words (
  (W::PLENTY
   (wordfeats (W::negatable +) (W::NOsimple +))
   (SENSES
    ((LF ONT::PLENTY)
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-TEMPL)
     (SYNTAX (W::agr W::3p) (w::status w::indefinite-plural))
     )
    )
   )
))

