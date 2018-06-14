;;;;
;;;; W::INTER
;;;;

;(define-words :pos w::adv
; :words (
;  (w::inter
;  (senses
;   ((lf-parent ont::BETWEEN)
;    (example "")
;    (templ PRED-VP-PRE-templ)
;    )
;   )
;  )
;))

(define-words :pos W::adj 
 :words (
  (W::inter-
   (SENSES
    (
     (LF-PARENT ONT::BETWEEN)
     (example "intercampus mail")
     ;(TEMPL central-adj-templ)
     (TEMPL prefix-adj-templ)
    )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::inter-
  (senses
   ((lf-parent ont::BETWEEN)
    (example "intercellular; interoperable")
    ;(templ ADJ-OPERATOR-TEMPL)
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))

; no applicable sense of personal to handle this compositionally
; interpersonal (unhyphenated) is mapped via WordFinder
(define-words :pos w::adj
 :words (
  ((w::inter w::punc-minus w::personal) 
  (senses
   ((lf-parent ont::social-interaction-val)
    (templ central-adj-TEMPL)
    )
   )
  )
))
