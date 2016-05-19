;;;;
;;;; W::low
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::low w::end)
    (SENSES
     ((meta-data :origin calo :entry-date 20041116 :change-date 20061106 :comments caloy2 :comlex nil)
      (example "a good book")
      (LF-PARENT ONT::substandard-VAL)
      (TEMPL central-adj-templ)
      )
     )
    )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LOW
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ;;;; need to differentiate between the two senses
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("low%3:00:01"))
     (EXAMPLE "a low ceiling")
     (LF-PARENT ONT::linear-dimension)
     )
    ((EXAMPLE "Give me lower resolution" "a low salt diet")
     (LF-PARENT ONT::weak)
     (sem (f::gradability +) (f::intensity ont::lo) (f::orientation ont::less))
     (TEMPL LESS-ADJ-TEMPL)
     (meta-data :origin boudreaux :entry-date 20031024 :change-date 20090731 :wn ("low%3:00:02") :comments nil)
     )
    )
   )
))

