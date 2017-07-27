;;;;
;;;; W::MULTI
;;;;

(define-words :pos w::adv
 :words (
  (w::multi-
  (senses
   ((lf-parent ont::poly-val)
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
   ((lf-parent ont::poly-val)
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
   ((lf-parent ont::poly-val)
    (example "multichromatic")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
