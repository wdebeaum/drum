;;;;
;;;; W::INTRA
;;;;

;(define-words :pos w::adv
; :words (
;  (w::intra
;  (senses
;   ((lf-parent ont::IN-LOC)
;    (example "")
;    (templ PRED-VP-PRE-templ)
;    )
;   )
;  )
;))

#|
(define-words :pos w::adv
 :words (
  (w::hyper-
  (senses
   ((lf-parent ont::DEGREE-MODIFIER-HIGH)
    (example "hyperactivate")
    (templ V-PREFIX-templ)
    )
   )
  )
))
|#

(define-words :pos W::adj 
 :words (
  (W::intra-
   (SENSES
    (
     (LF-PARENT ONT::IN-LOC)
     (example "intracampus mail")
     ;(TEMPL central-adj-templ)
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::intra-
  (senses
   ((lf-parent ont::IN-LOC)
    (example "intracellular")
    ;(templ ADJ-OPERATOR-TEMPL)
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
