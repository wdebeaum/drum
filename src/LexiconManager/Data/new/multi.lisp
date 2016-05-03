;;;;
;;;; W::MULTI
;;;;

(define-words :pos w::adv
 :words (
  (w::multi-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "multiubiquitinate")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::multi-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "multiubiquitination; multipeptide")
    (templ central-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::multi-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "multichromatic")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
