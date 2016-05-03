;;;;
;;;; W::HYPO
;;;;

(define-words :pos w::adv
 :words (
  (w::hypo-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-LOW)
    (example "hypoactivate")
    (templ PRED-VP-PRE-templ)
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
     (TEMPL central-adj-templ)
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
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
