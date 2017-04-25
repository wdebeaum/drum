;;;;
;;;; W::principal
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::principal
   (SENSES
    ((LF-PARENT ONT::professional)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data :wn ("principal%1:18:00"))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::principal
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data :wn ("main%5:00:00:important:00") )
     (lf-parent ont::primary)
     )
    )
   )
))

