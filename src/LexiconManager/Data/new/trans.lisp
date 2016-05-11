;;;;
;;;; W::TRANS
;;;;

(define-words :pos w::adv
 :words (
  (w::trans-
  (senses
   ((lf-parent ont::ACROSS)
    (example "transphosphrylate")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::trans-
   (SENSES
    (
     (LF-PARENT ONT::ACROSS)
     (example "transatlantic")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::trans-
  (senses
   ((lf-parent ont::ACROSS)
    (example "transcontinental")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
