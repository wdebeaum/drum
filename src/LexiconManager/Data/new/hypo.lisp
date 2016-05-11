;;;;
;;;; W::HYPO
;;;;

(define-words :pos w::adv
 :words (
  (w::hypo-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-LOW)
    (example "hypoactivate")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::hypo-
   (SENSES
    (
     (LF-PARENT ONT::DEGREE-MODIFIER-LOW)
     (example "hypoactivation")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::hypo-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-LOW)
    (example "hyposensitive")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
