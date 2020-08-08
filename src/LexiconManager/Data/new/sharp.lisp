;;;;
;;;; W::sharp
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::sharp
   (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     (LF-PARENT ONT::intense)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation F::neg))
     (example "a sharp pain")
     )
    )
   )
))

