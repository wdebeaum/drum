;;;;
;;;; W::term
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::term
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("term%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (example "use the short term parking")
     (TEMPL other-reln-templ)
     )
    ((LF-PARENT ONT::Mathematical-term)
     (TEMPL count-pred-templ)
     (meta-data :origin lam :entry-date 20050706 :change-date nil :wn ("term%1:09:00" "term%1:10:01") :comments nil)
     )
    ((LF-PARENT ONT::requirements)
     (TEMPL count-pred-3p-TEMPL)
     (syntax (W::morph (:forms (-none))))
     (example "he agreed to the terms of the contract")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("term%1:10:02") :comments caloy3)
     )
    ))
))

