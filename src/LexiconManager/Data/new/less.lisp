;;;;
;;;; W::LESS
;;;;

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
   (W::LESS
   (wordfeats (W::status ont::quantifier) (W::negatable +) (W::comparative +) (W::Mass ?m))
   (SENSES
    ((LF (:* ONT::QMODIFIER W::LESS)) ;; for some reason, LF-PARENT doesn't work here
     (example "less than seven" "less trucks than that" "less people" "less of the people" "less than seven of the people")
     (non-hierarchy-lf t)(TEMPL quan-than-comp)
     (SYNTAX (W::agr (? ag w::3s W::3p))) ;; can be singular -- less than one mile
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::LESS
   (SENSES
    ((LF-PARENT ONT::degree-modifier)
     (TEMPL PRED-S-POST-TEMPL)
     (example "Eat less.")
     )
    ((LF-PARENT ONT::LESS-VAL)
     ;(TEMPL COMPAR-THAN-templ)
     (TEMPL COMPAR-templ)
     )
    )
   )
  ))

#|
(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::at W::least)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::LESS-THAN)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))
|#

