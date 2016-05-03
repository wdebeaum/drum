;;;;
;;;; W::bed
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::bed
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("bed%1:06:00"))
     (LF-PARENT ont::furnishings)
     (SEM (F::mobility F::non-self-moving))
     )
    )
   )
))

(define-words :pos w::N :templ count-pred-templ
 :words (
    ((W::bed w::and w::breakfast)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060503 :change-date nil :wn ("bed_and_breakfast%1:06:00") :comment pq387)
     (LF-PARENT ont::bedandbreakfast)
     )
    )
   )
))

