;;;;
;;;; W::tick
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::tick w::mark)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::tick W::marks))))
   (SENSES
    ((LF-PARENT ONT::graphic-symbol)
     (EXAMPLE "add tick marks to the value axis")
     (meta-data :origin task-learning :entry-date 20050817 :change-date nil :comments nil)
     )
    )
   )
))

