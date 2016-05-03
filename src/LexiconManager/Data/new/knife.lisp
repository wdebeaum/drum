;;;;
;;;; W::knife
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::knife
    (wordfeats (W::morph (:forms (-S-3P) :plur w::knives)))
    (SENSES
     ((LF-PARENT ont::cutlery)
      (TEMPL count-pred-templ)
      (example "cut it with a knife")
      (meta-data :origin calo-ontology :entry-date 20060630 :change-date nil :wn ("knife%1:06:00") :comment nil)
      )
     )
    )
))

