;;;;
;;;; W::aerobics
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::aerobics
  (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::cardiopulmonary-exercise)
     ; changed to new type cardiopulmonary-exercise for asma
     (meta-data :origin cardiac :entry-date 20090422 :change-date 20111003 :wn ("workout%1:04:00") :comments calo-ontology)
     (templ mass-pred-templ)
     )
    )
   )
))

