;;;;
;;;; W::bio
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::bio
   (SENSES
    ((LF-PARENT ONT::BIOLOGICAL)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::bio-
  (senses
   ((lf-parent ont::BIOLOGICAL)
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
   ((lf-parent ont::BIOLOGICAL)
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
   ((lf-parent ont::BIOLOGICAL)
    (example "biophysical")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
