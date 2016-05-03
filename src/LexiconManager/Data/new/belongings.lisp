;;;;
;;;; W::belongings
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::belongings
    (wordfeats (W::morph (:forms (-none))))
   (SENSES
     ((LF-PARENT ONT::possession)
      (TEMPL count-pred-3p-TEMPL)
     (EXAMPLE "don't steal my belongings")
     (meta-data :origin trips :entry-date 20090401 :change-date nil :wn ("property%1:21:00") :comments nil)
     )
    )
   )
))

