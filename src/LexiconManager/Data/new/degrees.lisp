;;;;
;;;; W::DEGREES
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::DEGREES w::celsius)
    (wordfeats (W::morph (:forms (-none))))
    (SENSES
     ((LF-PARENT ONT::temperature-unit)
      (TEMPL ATTRIBUTE-UNIT-TEMPL)
      (meta-data :origin plow :entry-date 20060615 :change-date nil :comments pq)
      (example "five degrees celsius")
      )
     )
    )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::DEGREES w::fahrenheit)
    (wordfeats (W::morph (:forms (-none))))
    (SENSES
     ((LF-PARENT ONT::temperature-unit)
      (TEMPL ATTRIBUTE-UNIT-TEMPL)
      (meta-data :origin plow :entry-date 20060615 :change-date nil :comments pq)
      (example "five degrees fahrenheit")
      )
     )
    )
))

