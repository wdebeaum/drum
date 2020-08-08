;;;;
;;;; W::WORSE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::WORSE
   (wordfeats (W::COMPARATIVE +));  (W::FUNCTN ONT::ACCEPTABILITY-VAL))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20080918 :comments nil :wn ("worse%3:00:00"))
     (LF-PARENT ONT::LESS-VAL)
     (lf-form w::bad)
     (SEM (f::gradability +) (f::orientation F::neg) (f::intensity ont::med) (F::SCALE ONT::ACCEPTABILITY-VAL))
     (TEMPL COMPAR-TEMPL)
     )
    )
   )
))

