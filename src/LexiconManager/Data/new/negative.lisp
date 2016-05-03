;;;;
;;;; W::negative
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::negative
   (SENSES
    ((LF-PARENT ONT::IMAGE)
     (TEMPL OTHER-RELN-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("negative%1:06:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::NEGATIVE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("negative%3:00:03") :comments html-purchasing-corpus)
     (LF-PARENT ONT::polarity-VAL-NEGATIVE)
     )
    )
   )
))

