;;;;
;;;; W::opaque
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::opaque
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20050418 :change-date nil :wn ("opaque%3:00:00") :comments projector-purchasing)
     (LF-PARENT ONT::opaque-val)
     (example "an opaque projector")
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::opaque
  (senses
   ((LF-PARENT ONT::unclear)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation F::neg))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :comments nil)
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
  )
))

