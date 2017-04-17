;;;;
;;;; W::NEARBY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::NEARBY)
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("nearby%5:00:00:near:00"))
     (EXAMPLE "they went to nearby schools")
     (LF-PARENT ONT::near)
     (TEMPL ADJ-THEME-TEMPL)
     (SEM (f::orientation ont::less) (f::intensity ont::hi))
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::NEARBY
   (SENSES
    ((LF-PARENT ont::near-reln );ONT::APPROXIMATE-AT-LOC)
     (LF-FORM W::NEAR)
     (TEMPL PRED-S-TEMPL)
     )
    ((LF-PARENT ont::near-reln );ONT::APPROXIMATE-AT-LOC)
     (LF-FORM W::NEAR)
     (TEMPL PRED-NP-TEMPL)
     )
    )
   )
))

