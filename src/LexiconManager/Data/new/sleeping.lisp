;;;;
;;;; W::sleeping
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::sleeping W::pill)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::sleeping W::pills))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("sleeping_pill%1:06:00"))
     (LF-PARENT ONT::MEDICATION)
     )
    )
   )
))

