;;;;
;;;; W::magazine
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::magazine
   (SENSES
    ((LF-PARENT ONT::publication)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::magazine
   (SENSES
    ((meta-data :origin calo :entry-date 20050325 :change-date nil :wn ("magazine%1:10:00") :comments caloy2)
     (LF-PARENT ONT::info-medium)
     (example "i read it in a magazine")
     )
    )
   )
))

