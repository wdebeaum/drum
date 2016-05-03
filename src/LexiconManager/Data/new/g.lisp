;;;;
;;;; w::g
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::g
   (wordfeats (W::morph (:forms (-s-3p) :plur W::g)))
   (SENSES
    ((LF-PARENT ONT::weight-unit)
     (LF-FORM W::gram)
     (TEMPL UNIT-TEMPL)
     (meta-data :origin cernl :entry-date 20100607 :change-date nil :comments nil :wn ("gram%1:23:00"))
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::G
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

