;;;;
;;;; W::EXTRA
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::EXTRA
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("extra%5:00:00:unnecessary:00"))
     (LF-PARENT ONT::relative-quantity-val)
     )
    )
   )
))

;(define-words :pos w::adv
; :words (
;  (w::extra
;  (senses
;   ((lf-parent ont::OUTSIDE)
;    (example "")
;    (templ PRED-VP-PRE-templ)
;    )
;   )
;  )
;))

;(define-words :pos W::adj 
; :words (
;  (W::extra
;   (SENSES
;    (
;     (LF-PARENT ONT::OUTSIDE)
;     (example "")
;     (TEMPL central-adj-templ)
;     )
;    )
;   )
;))

(define-words :pos w::adv
 :words (
  (w::extra-
  (senses
   ((lf-parent ont::OUTSIDE)
    (example "extracellular")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))

