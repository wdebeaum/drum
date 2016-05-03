;;;;
;;;; W::cup
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::cup
   (SENSES
    ((LF-PARENT ONT::volume-measure-unit)
     (TEMPL classifier-mass-templ)
     (example "how many calories are in a cup of carrots")
     (meta-data :origin foodkb :entry-date 20050805 :change-date 20090521 :wn ("cup%1:23:02") :comment nil)
     )
    ((LF-PARENT ONT::cup)
     (TEMPL count-pred-templ)
     (example "hand me the cups")
     )
    #||((LF-PARENT ONT::cup)
     (TEMPL pred-subcat-contents-templ)
     (example "a cup of coffee")
     (meta-data :origin calo-ontology :entry-date 20060630 :change-date nil :wn ("cup%1:23:01") :comment nil)
     )||#
    )
   )
))

