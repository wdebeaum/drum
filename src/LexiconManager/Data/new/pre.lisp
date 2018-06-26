;;;;
;;;; W::PRE
;;;;

(define-words :pos w::adv
 :words (
  (w::pre-
  (senses
   ((lf-parent ont::before)
    (example "precook")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::pre-
   (SENSES
    (
     (LF-PARENT ONT::before)
     (example "preshow, predinner")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::pre-
  (senses
   ((lf-parent ont::before)
    (example "preadolescent")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
