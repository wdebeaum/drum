;;;;
;;;; W::SEMI
;;;;

(define-words :pos w::adj
 :words (
  (w::semi-
  (senses
   ((lf-parent ont::degree-modifier-med)
    (example "semi-darkness")
     (TEMPL prefix-adj-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::semi-
  (senses
   ((lf-parent ont::degree-modifier-med)
    (example "semi-independent")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
