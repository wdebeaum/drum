;;;;
;;;; W::kilo
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::kilo
;   (abbrev w::kg)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080215 :change-date nil :comments nil :wn ("kilo%1:23:00"))
     ;(LF-PARENT ONT::weight-unit)
     (LF-PARENT ONT::kg)
     (TEMPL SUBSTANCE-UNIT-TEMPL)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (w::kilo
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

