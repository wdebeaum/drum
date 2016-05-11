;;;;
;;;; W::MONO
;;;;

(define-words :pos w::adv
 :words (
  (w::mono-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "monoubiquitinate")
    (templ V-PREFIX-templ)
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
    (templ prefix-adj-templ)
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
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
