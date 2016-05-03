;;;;
;;;; W::cerebral
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::cerebral
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (LF-PARENT ONT::mental-VAL)
     (example "mental activity")
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::cerebral
  (senses
   ((LF-PARENT ONT::body-part-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

