;;;;
;;;; W::ACCEPTABLE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ACCEPTABLE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("acceptable%3:00:00"))
     (LF-PARENT ONT::good)
     (SEM (F::GRADABILITY F::+) (f::orientation ont::more) (f::intensity ont::med))
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

