;;;;
;;;; W::REGULAR
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::REGULAR
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("regular%3:00:00"))
     (LF-PARENT ONT::typical-val)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20040504 :change-date 20090731 :wn ("regular%5:00:00:scheduled:00") :comments calo-y1variants)
     (LF-PARENT ONT::REGULAR)
     (example "you can pay in regular installments")
     )
    )
   )
))

