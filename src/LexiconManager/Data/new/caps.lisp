;;;;
;;;; W::caps
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::caps
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::font)
     (templ mass-pred-templ)
     (meta-data :origin task-learning :entry-date 20050930 :change-date nil :comments nil)
     (example "Choose All Caps to make text all capital letters")
     )
    )
   )
))

