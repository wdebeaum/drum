;;;;
;;;; W::bio
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::bio
   (SENSES
    ((LF-PARENT ONT::BIOLOGY-VAL)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::bio-
  (senses
   ((lf-parent ONT::BIOLOGY-VAL)
    (example "biodegrade")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos w::adj
 :words (
  (w::bio-
  (senses
   ((lf-parent ONT::BIOLOGY-VAL)
    (templ prefix-adj-templ)
    (example "biosensor")
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::bio-
  (senses
   ((lf-parent ONT::BIOLOGY-VAL)
    (example "biophysical")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
