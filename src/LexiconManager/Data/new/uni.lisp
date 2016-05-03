;;;;
;;;; W::UNI
;;;;

(define-words :pos w::adv
 :words (
  (w::uni-
  (senses
   ((lf-parent ont::CARDINALITY-VAL)
    (example "uniprocessing")
    (templ PRED-VP-PRE-templ)
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
    (templ central-adj-templ)
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
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
