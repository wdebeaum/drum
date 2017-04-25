;;;;
;;;; w::old
;;;; 

(define-words :pos W::n
 :words (
  ((w::old w::age)
  (senses
   ((LF-PARENT ONT::lifecycle-stage)
    (TEMPL bare-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
    (W::OLD
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("old%5:00:00:preceding:00"))
     (lf-parent ont::old-val)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::old w::fashioned)
   (SENSES
    ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
     (LF-PARENT ONT::not-current-val)
     )
    )
   )
))

