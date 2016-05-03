;;;;
;;;; w::de
;;;;

; from "un-"
;(define-words :pos w::adv
; :words (
;  (w::de
;  (senses
;   ((lf-parent ont::NEG)
;    (example "unabridged")
;    (templ PRED-VP-PRE-templ)
;    )
;   )
;  )
;))

(define-words :pos w::adv
 :words (
  (w::de-
  (senses
   ((lf-parent ont::MANNER-UNDO)
    (example "deactivate; dephosphorylate")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::de-
   (SENSES
    (
     (LF-PARENT ONT::MANNER-UNDO)
     (example "deactivation")
     (TEMPL central-adj-templ)
     )
    )
   )
))

;(define-words :pos w::adv
; :words (
;  (w::de
;  (senses
;   ((lf-parent ont::NEG)
;    (example "")
;    (templ ADJ-OPERATOR-TEMPL)
;    )
;   )
;  )
;))
