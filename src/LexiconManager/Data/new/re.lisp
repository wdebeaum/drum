;;;;
;;;; w::re
;;;;

(define-words :pos W::adv 
 :words (
  (W::re-
   (SENSES
    ((LF-PARENT ONT::REPETITION)
    (example "reactivate")
    (templ V-PREFIX-templ)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (W::re-
   (SENSES
    ((LF-PARENT ONT::REPETITION)
    (example "reactivation")
    (templ prefix-adj-templ)
     )
    )
   )
))

;(define-words :pos w::adv
; :words (
;  (w::re
;  (senses
;   ((lf-parent ont::REPETITION)
;    (example "")
;    (templ ADJ-OPERATOR-TEMPL)
;    )
;   )
;  )
;))
