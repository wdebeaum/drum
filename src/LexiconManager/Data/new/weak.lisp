;;;;
;;;; W::WEAK
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::WEAK
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20050216 :change-date 20090731 :wn ("weak%3:00:00") :comments caloy2)
      (LF-PARENT ONT::weak)
      (sem (f::gradability +) (f::intensity ont::lo) (f::orientation ont::less))
      (TEMPL LESS-ADJ-TEMPL)
      )
     )
    )
))

