;;;;
;;;; W::BEST
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::BEST
   (wordfeats (W::COMPARATIVE W::SUPERL)); (W::FUNCTN ONT::ACCEPTABILITY-VAL))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("best%3:00:00"))
     (LF-PARENT ONT::max-val)
     (lf-form w::good)
     (TEMPL SUPERL-TEMPL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi) (F::SCALE ONT::ACCEPTABILITY-VAL))
     )
    )
   )
))

