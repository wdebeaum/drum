;;;;
;;;; W::conventional
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::conventional
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin lam :entry-date 20050422 :change-date 20080818 :wn ("conventional%3:00:00") :comments lam-initial)
     (LF-PARENT ONT::conventionality-val) ;; changed from ont::uniqueness-val
     )
    ))
))

(define-words :pos W::adj
 :words (
  (w::conventional
  (senses
   ((LF-PARENT ONT::common)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::more))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :comments nil)
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
)
))

