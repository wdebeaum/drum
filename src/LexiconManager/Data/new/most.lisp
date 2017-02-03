;;;;
;;;; W::MOST
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::MOST
   (wordfeats (W::COMPARATIVE W::SUPERL)); (W::FUNCTN ONT::COMPARE-VAL))
   (SENSES
    ((LF-PARENT ONT::MAX-VAL)
     (TEMPL SUPERL-TEMPL (xp (% W::pp (W::ptype W::of))))
     (meta-data :origin calo :entry-date 20050505 :change-date nil :wn ("most%3:00:02") :comments projector-purchasing)
     (example "the most fun I've had in years")
     (SEM (F::SCALE ONT::COMPARE-VAL))
     )
    )
  )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::most
   (wordfeats (W::negatable +))
   (SENSES
    ((LF ONT::most)   ;; the quantity interp of most, e.g., most trucks
     (non-hierarchy-lf t)
     (TEMPL quan-cardinality-pl-templ)
     (syntax (w::status ont::indefinite-plural))
     )
    ((LF ONT::MOST) ;; mass sense of most, e.g., most WATER
     (non-hierarchy-lf t)(TEMPL quan-mass-TEMPL)
     (SYNTAX (W::agr (? agr W::3s)) (w::status ont::indefinite))
     )
    )
   )
))

