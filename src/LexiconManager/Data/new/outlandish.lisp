;;;;
;;;; W::outlandish
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::outlandish
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :wn ("strange%3:00:00") :comments LM-vocab)
     (EXAMPLE "A freaky idea")
     (lf-parent ont::exceptional-val)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation F::neg))
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::outlandish
  (senses
   ((lf-parent ont::exceptional-val)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation F::neg))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :wn ("strange%3:00:00") :comments nil)
    )
   )
)
))

