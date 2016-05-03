;;;;
;;;; W::uniform
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::uniform
   (SENSES
    ((LF-PARENT ONT::homogeneous-VAL)
     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("uniform%3:00:00" "uniform%5:00:00:homogeneous:00") :comments nil)
     (EXAMPLE "a uniform consistency")
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (w::uniform
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

