;;;;
;;;; W::mi
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ((W::mi) ;; alternate plural
   (SENSES
    ((LF-PARENT ONT::length-unit)
     (LF-FORM W::mile)
     (TEMPL ATTRIBUTE-UNIT-PLURAL-TEMPL)
     (example "the distance is 2.3 mi")
     (meta-data :origin plow :entry-date 20070709 :change-date 20070813 :comments caloy4)
     )
    )
   )
))

