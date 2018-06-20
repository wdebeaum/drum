;;;;
;;;; W::post
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::post w::doc)
   (SENSES
    ((meta-data :origin caloy3 :entry-date 20070330 :change-date nil :wn ("researcher%1:18:00") :comments caloy3-test-data)
     (LF-PARENT ONT::professional)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::post
   (SENSES
    ((LF-PARENT ONT::SEND)
     (meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date nil :comments nil :vn ("send-11.1") :wn ("post%2:32:02"))
     (example "post the website to the network")
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::post-
  (senses
   ((lf-parent ont::event-time-rel)
    (example "postprocess")
    (templ V-PREFIX-templ)
    )
   )
  )
))

(define-words :pos W::adj 
 :words (
  (W::post-
   (SENSES
    (
     (LF-PARENT ONT::event-time-rel)
     (example "postshow, postdinner")
     (TEMPL prefix-adj-templ)
     )
    )
   )
))

(define-words :pos w::adv
 :words (
  (w::post-
  (senses
   ((lf-parent ont::event-time-rel)
    (example "postadolescent")
    (templ adj-operator-prefix-TEMPL)
    )
   )
  )
))
