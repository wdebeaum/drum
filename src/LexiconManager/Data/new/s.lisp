;;;;
;;;; W::s
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::s
   (wordfeats (W::morph (:forms (-s-3p) :plur W::s)))
   (SENSES
    ((LF-PARENT ONT::time-unit)
     (LF-FORM W::second)
     (TEMPL UNIT-TEMPL)
     (meta-data :origin step :entry-date 20080711 :change-date nil :comments nil :wn ("s%1:28:00"))
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::S
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   
	  (PREFERENCE 0.97)
    )
   )
)
))

