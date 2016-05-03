;;;;
;;;; W::accent
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::accent w::mark)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::accent W::marks))))
   (SENSES
    ((LF-PARENT ONT::graphic-symbol)
     (EXAMPLE "add accent marks to characters")
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :wn ("accent_mark%1:10:00") :comments nil)
     )
    )
   )
))

