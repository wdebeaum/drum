;;;;
;;;; W::strong
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::strong
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20050216 :change-date 20090731 :wn ("strong%3:00:00") :comments caloy2)
      (LF-PARENT ONT::intense)
      (EXAMPLE "intense odor")
      (TEMPL central-ADJ-TEMPL)
      (sem (f::gradability +) (f::intensity ont::hi) (f::orientation F::pos))
      )
     ((LF-PARENT ONT::strong-val)
      (EXAMPLE "strong person" "strong pillars")
      )
     )
    )
))

