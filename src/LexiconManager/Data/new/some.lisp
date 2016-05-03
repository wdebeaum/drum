;;;;
;;;; W::SOME
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::SOME
   (wordfeats (W::negatable +))
   (SENSES
    ((LF W::SOME)   ;; the quantity interp of some, e.g., some trucks
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-templ)
     (SYNTAX (W::agr W::3p) (W::status W::indefinite-plural))   ;; must be plural if count
     )
    ((LF W::INDEFINITE)  ;; like "a" or "an", e.g., some man
     (non-hierarchy-lf t)
     (syntax (w::status w::indefinite))
     (TEMPL INDEFINITE-COUNTABLE-TEMPL)
     )
    ((LF W::SM) ;; mass sense of SOME, e.g., SOME WATER
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (SYNTAX (w::status w::sm) (W::agr W::3s)) ; never plural if mass
     )
    )
   )
))

