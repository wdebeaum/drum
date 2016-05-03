;;;;
;;;; w::un
;;;;

;(define-words :pos w::adv
; :words (
;  (w::un-
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
  (w::un-
  (senses
   ((lf-parent ont::MANNER-UNDO)
    (example "unbundle; unload; unbind; unclutter; unwrap")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::un-
   (SENSES
    (
     (LF-PARENT ONT::NEG)
     (example "unemployment")
     (TEMPL central-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::un-
  (senses
   ((lf-parent ont::NEG)
    (example "unfair")
    (templ ADJ-OPERATOR-TEMPL)
    )
   )
  )
))
