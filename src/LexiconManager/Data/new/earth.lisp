;;;;
;;;; W::earth
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::earth
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments nil)
     (LF-PARENT ONT::geo-formation)
     )
    ((meta-data :origin step :entry-date 20080623 :change-date nil :comments nil :wn ("earth%1:27:00" "earth%1:27:01"))
     ;(LF-PARENT ONT::substance) ;like dirt
     (LF-PARENT ONT::earth-substance)
     )
    )
   )
))

(define-words :pos W::name
 :words (
  (w::earth
  (senses((LF-PARENT ONT::planet)
    (TEMPL nname-templ)
    )
   )
)
))

