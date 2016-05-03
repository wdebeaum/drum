;;;;
;;;; W::HETERO
;;;;

(define-words :pos w::adv
 :words (
  (w::hetero-
  (senses
   ((lf-parent ont::DIFFERENT)
    (example "heterodimerize")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::hetero-
   (SENSES
    (
     (LF-PARENT ONT::DIFFERENT)
     (example "heterodimer")
     (TEMPL central-adj-templ)
     )
    )
   )
))

;(define-words :pos w::adv
; :words (
;  (w::hetero
;  (senses
;   ((lf-parent ont::DIFFERENT)
;    (example "")
;    (templ ADJ-OPERATOR-TEMPL)
;    )
;   )
;  )
;))
