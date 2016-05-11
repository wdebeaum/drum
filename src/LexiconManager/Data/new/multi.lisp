;;;;
;;;; W::MULTI
;;;;

(define-words :pos w::adv
 :words (
  (w::multi-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "multiubiquitinate")
    (templ V-PREFIX-templ)
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
    (templ prefix-adj-templ)
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
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
