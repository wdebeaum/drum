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

    ((lf-parent ont::level)
     (example "the degree of coldness")
     (TEMPL other-reln-templ)
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :wn ("level%1:07:00") :comments nil)
     )	  
    ((lf-parent ont::level)
     (example "the degree of 5")
     (TEMPL reln-subcat-of-number-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :wn ("level%1:07:00") :comments nil)
     )	  
    
    )
   )
))

