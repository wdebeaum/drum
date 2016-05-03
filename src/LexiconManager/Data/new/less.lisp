;;;;
;;;; W::LESS
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
   (W::LESS
   (wordfeats (W::status W::quantifier) (W::negatable +) (W::comparative +) (W::Mass ?m))
   (SENSES
    ((LF (:* ONT::QMODIFIER W::LESS)) ;; for some reason, LF-PARENT doesn't work here
     (example "less than seven" "less trucks than that" "less people" "less of the people" "less than seven of the people")
     (non-hierarchy-lf t)(TEMPL quan-than-comp)
     (SYNTAX (W::agr (? ag w::3s W::3p))) ;; can be singular -- less than one mile
     )
    )
   )
))

