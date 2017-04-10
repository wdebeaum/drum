;;;;
;;;; W::SLIGHT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SLIGHT
   (wordfeats (W::MORPH (:FORMS (-ER ))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("slight%3:00:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::small)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((LF-PARENT ONT::mild-VAL)
     (meta-data :origin cardiac :entry-date 20080429  :change-date nil :wn ("oppressive%5:00:00:domineering:00") :comments nil)
     (sem (f::gradability +) (f::intensity ont::lo) (f::orientation ont::less))
     (example "a slight headache")
     )
    )
   )
))

