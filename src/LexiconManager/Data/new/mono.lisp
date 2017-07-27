;;;;
;;;; W::MONO
;;;;

(define-words :pos w::adv
 :words (
  (w::mono-
  (senses
   ((lf-parent ont::mono-val)
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
   ((lf-parent ont::mono-val)
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
   ((lf-parent ont::mono-val)
    (example "monochromatic")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
