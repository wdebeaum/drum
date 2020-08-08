;;;;
;;;; w::fat
;;;;

(define-words :pos W::n
 :words (
  (w::fat
  (senses
   ((LF-PARENT ONT::substance)
    (TEMPL mass-PRED-TEMPL)
    (meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
    (example "high cholesterol")
    )
   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::fat
    (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080730 :change-date 20090731 :comments speech-pretest)
     (lf-parent ont::fat-val)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation F::pos))
     (example "a fat cat" "a fat line")
     )
    )
   )
))

