;;;;
;;;; W::LB
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::LB 
   (wordfeats (W::morph (:forms (-s-3p) :plur W::lb)))
   (SENSES
    ((meta-data :origin calo :entry-date 20060803 :change-date nil :comments nil :wn ("lb%1:23:00"))
     (LF-PARENT ONT::weight-unit)     
     (TEMPL substance-UNIT-TEMPL)
     (example "he weighs 5 lb 10 oz")
     (LF-FORM W::pound)
     )
    )
   )
))

