;;;;
;;;; w::dis
;;;;

(define-words :pos w::adv
 :words (
  (w::dis-
  (senses
   ((lf-parent ont::NEG)
    (example "disallow; dislike")
    (templ PRED-VP-PRE-templ)
    )
   )
  )
))

(define-words :pos w::adv
 :words (
  (w::dis-
  (senses
   ((lf-parent ont::MANNER-UNDO)
    (example "disconnect; disintegrate")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::dis-
   (SENSES
    (
     (LF-PARENT ONT::NEG)
     (example "disbelief; discomfort")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

; from "non-"
;(define-words :pos w::adv
; :words (
;  (w::non
;  (senses
;   ((lf-parent ont::NEG)
;    (example "nonspecific; nonalcoholic")
;    (templ ADJ-OPERATOR-TEMPL)
;    )
;   )
;  )
;))
