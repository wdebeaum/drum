;;;;
;;;; W::subaddress
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::subaddress 
   (SENSES
    ((LF-PARENT ONT::location-id) ;; should use a different lf to distinguish from standard w::address
     (EXAMPLE "what is the contact record subaddress")
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     )
    )
   )
))

