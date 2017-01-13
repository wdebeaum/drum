;;;;
;;;; W::stenosis
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::stenosis
   (wordfeats (W::morph (:forms (-S-3P) :plur w::stenoses)))
   (SENSES
    ((meta-data :origin trips :entry-date 20090422 :change-date nil :comments nil)
     (LF-PARENT ONT::medical-condition)
     (templ mass-pred-templ)
     )
    )
   )
))

