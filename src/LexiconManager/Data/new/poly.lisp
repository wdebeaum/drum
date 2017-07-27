;;;;
;;;; W::POLY
;;;;

(define-words :pos w::adv
 :words (
  (w::poly-
  (senses
   ((lf-parent ont::poly-val)
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
   ((lf-parent ont::poly-val)
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
   ((lf-parent ont::poly-val)
    (example "polychromatic")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
