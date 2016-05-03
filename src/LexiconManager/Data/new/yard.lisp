;;;;
;;;; W::YARD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
(W::YARD
 (abbrev w::yd)
 (SENSES
  ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("yard%1:23:00"))
   (LF-PARENT ONT::length-unit)
   (TEMPL ATTRIBUTE-UNIT-TEMPL)
   (example "he bought three yards of cloth")
   )
  )
 )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
(w::yard
 (senses
  ((meta-data :origin calo-ontology :entry-date 20060707 :change-date nil :wn ("yard%1:06:02") :comments nil)
   (LF-PARENT ONT::structure-external-component)
   (templ gen-part-of-reln-templ)
   (example "he worked in the yard all weekend")
   )
  )
 )
))

