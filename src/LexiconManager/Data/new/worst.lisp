;;;;
;;;; W::WORST
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::WORST
   (wordfeats (W::COMPARATIVE W::SUPERL) (W::FUNCTN ONT::ACCEPTABILITY-VAL))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20080918 :comments nil :wn ("worst%3:00:00"))
     (LF-PARENT ONT::MIN-VAL)
     (lf-form w::bad)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL SUPERL-TEMPL)
     )
    )
   )
))

