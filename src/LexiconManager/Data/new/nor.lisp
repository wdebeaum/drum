;;;;
;;;; W::NOR
;;;;

(define-words :pos W::conj :boost-word t
 :words (
  (W::NOR
   (SENSES
    ((LF W::NOR)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-DISJ-TEMPL)
     (SYNTAX (w::subcat w::S) (w::operator w::none-of) (W::Neg +))
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil)
     )
    )
   )
))

