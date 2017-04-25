;;;;
;;;; w::common
;;;;

(define-words :pos W::adj
 :tags (:base500)
 :words (
  (w::common
  (senses
   ((lf-parent ont::typical-val)
    (TEMPL  adj-co-theme-templ (xp (% w::pp (w::PTYPE (? pt w::to)))))
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::more))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :comments nil)
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
)
))

