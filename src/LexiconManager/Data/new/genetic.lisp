;;;;
;;;; w::genetic
;;;;

(define-words :pos W::adj
 :words (
  (w::genetic
  (senses
   ((LF-PARENT ONT::body-system-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
   (W::genetic
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080520 :change-date nil :comments nil)
     (LF-PARENT ONT::hereditary)
     (example "a genetic disorder")
     (templ central-adj-templ)
     )
    )
   )
))

