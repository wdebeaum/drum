;;;;
;;;; W::oil
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::oil
   (SENSES
     ((LF-PARENT ONT::natural-liquid-substance)
     (TEMPL MASS-PRED-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments nil)
     (example "an oil well")
     )
     ((LF-PARENT ONT::fats-oils)
     (TEMPL BARE-PRED-TEMPL)
     (meta-data :origin foodkb :entry-date 20060425 :change-date nil :comments nil)
     (example "sesame is a popular cooking oil")
     (PREFERENCE 0.98) ; only when we need comestible
     )
    )
   )
))

