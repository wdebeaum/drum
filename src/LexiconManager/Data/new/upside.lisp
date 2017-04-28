;;;;
;;;; W::UPSIDE
;;;;

(define-words :pos W::n
 :words (
  ((W::UPSIDE W::DOWN W::CAKE)
  (senses
	   ((LF-PARENT ONT::CAKE-PIE)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::upside w::down)
   (SENSES
    ((LF-PARENT ONT::INVERTED-VAL)
     (EXAMPLE "flip the object upside down")
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
     )
    )
   )
))

