;;;;
;;;; W::mhz
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::mhz
   (wordfeats (W::morph (:forms (-s-3p) :plur W::mhz)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("mhz%1:28:00"))
     (LF-PARENT ONT::frequency-unit)
     (LF-FORM W::megahertz)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    )
   )
))

