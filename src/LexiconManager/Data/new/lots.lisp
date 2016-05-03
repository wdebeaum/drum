;;;;
;;;; w::lots
;;;;

(define-words :pos W::quan :boost-word t
 :words (
     (w::lots
    (wordfeats (W::negatable +) (W::NOsimple +))
    (SENSES
     ((LF W::PLENTY)
      (example "lots of water")
      (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
      (SYNTAX (W::agr (? agr W::3s)) (w::status w::indefinite))
      )
     ((LF W::PLENTY)
      (example "lots of people")
       (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-TEMPL)
      (SYNTAX (W::agr W::3p) (w::status w::indefinite-plural))
      )
     )
    )
))

