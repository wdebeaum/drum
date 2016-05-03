;;;;
;;;; W::euro
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::euro
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("euro%1:23:00"))
     (LF-PARENT ONT::money-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::euro
   (SENSES
    ((LF-PARENT ONT::money-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (EXAMPLE "the font contains the Euro currency symbol")
     (meta-data :origin task-learning :entry-date 20050826 :change-date nil :wn ("euro%1:23:00") :comments nil)
     )
    )
   )
))

