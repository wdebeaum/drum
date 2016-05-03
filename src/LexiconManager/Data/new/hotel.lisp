;;;;
;;;; W::hotel
;;;;

(define-words :pos w::N :templ count-pred-templ
 :words (
  (W::hotel
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :wn ("hotel%1:06:00") :comment caloy3)
     (LF-PARENT ont::accomodation)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (w::hotel
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

