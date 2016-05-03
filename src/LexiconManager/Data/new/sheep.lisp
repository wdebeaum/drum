;;;;
;;;; W::sheep
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::sheep
   (wordfeats (W::morph (:forms (-S-3P) :plur W::sheep)))
   (SENSES
    ((LF-PARENT ONT::nonhuman-animal)
     (EXAMPLE "wolf in sheep's clothing")
     (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :wn ("sheep%1:05:00") :comments caloy3)
     )
    )
   )
))

