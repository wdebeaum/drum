;;;;
;;;; W::wife
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::wife
     (wordfeats (W::morph (:forms (-s-3p) :plur w::wives)))
     (SENSES
      ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("wife%1:18:00") :comments nil)
       (LF-PARENT ONT::family-relation)
       (templ part-of-reln-templ)
       )
      )
     )
))

