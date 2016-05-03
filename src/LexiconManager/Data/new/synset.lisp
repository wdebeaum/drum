;;;;
;;;; W::synset
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::synset
   (SENSES
    ((LF-PARENT ONT::grouping)
     (TEMPL classifier-count-pl-templ)
     (example "a synset of words")
     (meta-data :origin calo-ontology :entry-date 20060523 :change-date nil :wn ("synset%1:14:00") :comments nil)
     )
    )
   )
))

