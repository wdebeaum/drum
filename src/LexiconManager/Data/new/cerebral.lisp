
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::cerebral
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (lf-parent ont::mental-val)
     (example "mental activity")
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::cerebral
  (senses
   ((lf-parent ont::mental-val)
    (TEMPL central-adj-templ)
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
   )
)
))

