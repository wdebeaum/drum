;;;;
;;;; W::approximate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::approximate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090522 :comments nil :vn ("price-54.4"))
     (LF-PARENT ONT::becoming-aware-of-value)
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::approximate
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040416 :change-date nil :wn ("approximate%5:00:00:inexact:00") :comments caloy2)
     (lf-parent ont::not-precise-val)
     )
    )
   )
))

