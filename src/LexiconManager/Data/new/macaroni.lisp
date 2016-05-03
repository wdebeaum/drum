;;;;
;;;; w::macaroni
;;;;

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
  (w::macaroni
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050801 :change-date nil :wn ("macaroni%1:13:00") :comments nil)
     (LF-PARENT ont::pasta)
     )
    )
   )


))

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
  ((w::macaroni w::and w::cheese)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin caet :entry-date 20130523 :change-date nil :comments icmi)
     (LF-PARENT ont::pasta)
     )
    )
   )
))

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
  ((w::mac w::and w::cheese)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin caet :entry-date 20130523 :change-date nil :comments icmi)
     (LF-PARENT ont::pasta)
     )
    )
   )
))
