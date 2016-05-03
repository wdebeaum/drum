;;;;
;;;; W::farthest
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::farthest
   (wordfeats (W::COMPARATIVE W::SUPERL) (W::FUNCTN ONT::linear-scale))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("farther%5:00:01:far:00"))
     (LF-PARENT ONT::max-val)
     (lf-form w::far)
     (TEMPL SUPERL-TEMPL)
     (SEM (f::gradability +) (f::orientation ont::more) (f::intensity ont::hi))
     )
    )
   )
))

