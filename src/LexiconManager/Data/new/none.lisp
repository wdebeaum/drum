;;;;
;;;; W::NONE
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::NONE
   (wordfeats  (W::nosimple +))
   (SENSES
    ((LF W::NONE)
     (example "none of the trucks")
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-templ)
     (SYNTAX (W::agr (? agr W::3p)) (w::status w::indefinite-plural))
     )
    ((LF W::NONE)
     (example "none of the water")
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (SYNTAX (W::agr (? agr W::3s)) (w::status w::indefinite)) ; -- never plural if mass
     )
    )
   )
))

