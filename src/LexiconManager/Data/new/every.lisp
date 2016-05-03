;;;;
;;;; W::EVERY
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::EVERY
   (wordfeats (W::agr W::3s) (W::mass W::count) (W::status W::quantifier) (W::negatable +))
   (SENSES
    ((LF W::EVERY)
     (non-hierarchy-lf t)(TEMPL quan-no-bare-TEMPL)
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :words (
  ((W::EVERY W::OTHER)
   (wordfeats (W::agr W::3s) (W::mass W::count) (W::status W::quantifier) (W::negatable +))
   (SENSES
    ((LF W::EVERY-OTHER)
     (non-hierarchy-lf t)(TEMPL quan-no-bare-TEMPL)
     )
    )
   )
))

