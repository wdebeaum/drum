;;;;
;;;; w::primary
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::primary W::CARE)
   (SENSES
    ((meta-data :origin plot :entry-date 20081013 :change-date nil :comments nil :wn ("care%1:04:01"))
     (LF-PARENT ONT::care)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::primary
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040504 :change-date 20090731 :wn ("primary%3:00:00") :comments calo-y1variants)
     (LF-PARENT ONT::primary)
     )
    )
   )
))

