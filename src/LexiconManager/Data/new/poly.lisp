;;;;
;;;; W::POLY
;;;;

(define-words :pos w::adv
 :words (
  (w::poly-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "polyubiquitinate")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::poly-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "polyubiquitination; polypeptide")
    (templ prefix-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::poly-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "polychromatic")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
