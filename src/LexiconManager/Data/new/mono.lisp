;;;;
;;;; W::MONO
;;;;

(define-words :pos w::adv
 :words (
  (w::mono-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "monoubiquitinate")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::mono-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "monoubiquitination; monopeptide")
    (templ central-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::mono-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "monochromatic")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
