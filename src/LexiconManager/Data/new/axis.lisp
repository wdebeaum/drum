;;;;
;;;; W::axis
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::axis
   (wordfeats (W::morph (:forms (-S-3P) :plur w::axes)))
   (SENSES
    ((LF-PARENT ONT::graphic-symbol)
     (EXAMPLE "position tick marks along axes")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :wn ("axis%1:09:00") :comments nil)
     (TEMPL other-reln-templ)
     )
    )
   )
))

