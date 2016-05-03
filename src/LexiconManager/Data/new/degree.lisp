;;;;
;;;; W::DEGREE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DEGREE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("degree%1:23:03"))
     (LF-PARENT ONT::temperature-unit)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    ((LF-PARENT ONT::angle-unit)
     (meta-data :origin fruitcarts :entry-date 20041117 :change-date nil :wn ("degree%1:23:00") :comments nil)
     (example "move it 45 degrees")
     (TEMPL attribute-UNIT-TEMPL)
     )
    )
   )
))

