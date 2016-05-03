;;;;
;;;; W::FEWER
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::FEWER
   (wordfeats (W::status W::indefinite-plural) (W::negatable +) (W::comparative +) (W::Mass W::COUNT))
   (SENSES
    ((LF (:* ONT::QMODIFIER W::LESS)) ;; for some reason, LF-PARENT doesn't work here
     (example "fewer than seven" "fewer trucks than that" "fewer people" "fewer of the people" )
     (non-hierarchy-lf t)
     (TEMPL quan-than-comp)
     (SYNTAX (W::agr W::3p))
     )
    )
   )
))

