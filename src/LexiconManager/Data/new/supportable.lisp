;;;;
;;;; W::supportable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::supportable
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::good)
     (SEM (F::GRADABILITY F::+) (f::orientation ont::less) (f::intensity ont::lo))
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
  )
))

