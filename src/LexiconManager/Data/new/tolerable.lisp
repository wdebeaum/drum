;;;;
;;;; W::tolerable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::tolerable
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080222 :change-date 20090731 :comments nil)
     (lf-parent ont::tolerable-val)
     (SEM (F::GRADABILITY +) (f::orientation F::pos) (f::intensity ont::med))
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
  )
))

