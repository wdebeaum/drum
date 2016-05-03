;;;;
;;;; W::w
;;;;

(define-words :pos w::N :templ count-pred-templ
 :words (
   ((W::w w::c)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::w W::cs))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :comment caloy3)
     (LF-PARENT ONT::internal-enclosure)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::W
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

