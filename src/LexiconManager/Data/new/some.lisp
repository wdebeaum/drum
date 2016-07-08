;;;;
;;;; W::SOME
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::SOME
   (wordfeats (W::negatable +))
   (SENSES
    ((LF ONT::SOME)   ;; the quantity interp of some, e.g., some trucks
     (non-hierarchy-lf t)(TEMPL quan-cardinality-pl-templ)
     (SYNTAX (W::agr W::3p) (W::status ont::indefinite-plural))   ;; must be plural if count
     )
    ((LF ONT::INDEFINITE)  ;; like "a" or "an", e.g., some man
     (non-hierarchy-lf t)
     (syntax (w::status ont::indefinite))
     (TEMPL INDEFINITE-COUNTABLE-TEMPL)
     )
    ((LF ONT::SM) ;; mass sense of SOME, e.g., SOME WATER
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (SYNTAX (w::status ont::sm) (W::agr W::3s)) ; never plural if mass
     )
    )
   )
))

