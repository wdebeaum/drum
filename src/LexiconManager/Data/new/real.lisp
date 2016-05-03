;;;;
;;;; W::real
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::real
   (SENSES
    ((meta-data :origin calo :entry-date 20040915 :change-date 20090915 :wn ("real%5:00:00:true:00") :comments caloy2)
     (LF-PARENT ONT::actual)
     (example "coffee with real cream")
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::REAL
   (SENSES
    ((LF-PARENT ONT::degree-MODIFIER-HIGH)
     (LF-FORM W::VERY)
     (TEMPL ADJ-OPERATOR-TEMPL)
     )
    )
   )
))

