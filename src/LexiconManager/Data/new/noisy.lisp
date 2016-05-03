;;;;
;;;; W::NOISY
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::NOISY
    (wordfeats (W::MORPH (:FORMS (-ER -LY))))
    (SENSES
     ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("noisy%3:00:00") :comments html-purchasing-corpus)
      (LF-PARENT ONT::NOISY)
      (example "they booked him in a noisy room facing the street")
      (TEMPL central-ADJ-TEMPL)
      )
     ((meta-data :origin lou2 :entry-date 20061121 :change-date nil :comments nil)
      (LF-PARENT ONT::interference-val)
      (example "ivan has a noisy sensor")
      (TEMPL central-ADJ-TEMPL)
      )
     )
    )
))

