;;;;
;;;; w::m
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::m W::g)
    (wordfeats (W::morph (:forms (-s-3p) :plur (W::m w::g))))
   (SENSES
    ((LF-PARENT ONT::weight-unit)
     (LF-FORM W::milligram)
     (TEMPL UNIT-TEMPL)
     (meta-data :origin cernl :entry-date 20100607 :change-date nil :comments nil :wn ("gram%1:23:00"))
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::m ;; alternate plural
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    (;(LF-PARENT ONT::length-unit)
     ;(LF-FORM W::meter)
     (LF-PARENT ONT::m)
     (TEMPL ATTRIBUTE-UNIT-PLURAL-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("m%1:23:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::m w::m)
   (SENSES
    (;(LF-PARENT ONT::length-unit)
     (LF-PARENT ONT::mm)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((W::m w::p w::h)
   (SENSES
    ( (meta-data :origin calo :entry-date 20050522 :change-date nil :comments caloy2)
      (lf-parent ont::rate-unit)
      (templ bare-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
      ((W::m w::p w::g)
   (SENSES
    ( (meta-data :origin calo :entry-date 20050522 :change-date nil :comments caloy2)
      (lf-parent ont::rate-unit)
      (templ bare-pred-templ)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::M
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::M-2)
   (SENSES
     ((LF-PARENT ONT::m2)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     )
    )
   )
))
