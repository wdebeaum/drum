;;;;
;;;; W::parenthesis
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::parenthesis
   (wordfeats (W::morph (:forms (-S-3P) :plur W::parentheses)))
   (SENSES
    ((LF-PARENT ONT::letter-symbol) ; graphic-symbol?
     (EXAMPLE "use parentheses to refine your search")
     (meta-data :origin task-learning :entry-date 20050919 :change-date nil :wn ("parenthesis%1:10:00") :comments nil)
     )
    )
   )
))

