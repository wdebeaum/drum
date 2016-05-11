;;;;
;;;; W::UNI
;;;;

(define-words :pos w::adv
 :words (
  (w::uni-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "uniprocessing")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::uni-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "uniprocessor; unicolor")
    (templ prefix-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::uni-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "unilingual")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
