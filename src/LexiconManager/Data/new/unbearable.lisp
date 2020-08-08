;;;;
;;;; W::UNbearable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::UNbearable
       (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080222 :change-date 20090731 :comments nil)
     (lf-parent ont::not-tolerable-val)
     (SEM (F::GRADABILITY +) (f::orientation F::neg) (f::intensity ont::hi))
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

