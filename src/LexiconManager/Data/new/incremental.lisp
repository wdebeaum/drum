;;;;
;;;; w::incremental
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (w::incremental
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((lf-parent ont::incremental-val)
     (meta-data :origin plot :entry-date 20080529 :change-date nil :comments nil)
     (example "incremental processing")
     )
    )
   )
))

