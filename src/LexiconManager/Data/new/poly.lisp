;;;;
;;;; W::POLY
;;;;

(define-words :pos w::adv
 :words (
  (w::poly-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "polyubiquitinate")
    (templ PRED-VP-PRE-templ)
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
    (templ central-adj-templ)
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
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
