;;;;
;;;; W::PRE
;;;;

(define-words :pos w::adv
 :words (
  (w::pre-
  (senses
   ((lf-parent ont::event-time-rel)
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
     (LF-PARENT ONT::event-time-rel)
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
   ((lf-parent ont::event-time-rel)
    (example "preadolescent")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
