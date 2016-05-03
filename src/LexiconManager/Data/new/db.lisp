;;;;
;;;; W::db
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::db 
   (wordfeats (W::morph (:forms (-s-3p) :plur W::db)))
   (SENSES
    ((meta-data :origin calo :entry-date 20050308 :change-date nil :wn ("db%1:23:00") :comments projector-purchasing)
     (LF-PARENT ONT::acoustic-unit)
     (LF-FORM W::decibel)
     (TEMPL attribute-UNIT-TEMPL)
     (example "it has an audible noise level of 35 db")
     )
    )
   )
))

