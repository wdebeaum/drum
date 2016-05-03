;;;;
;;;; w::pie
;;;;

(define-words :pos W::n
 :words (
  (w::pie
  (senses
	   ((LF-PARENT ONT::CAKE-PIE)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pie
   (SENSES
    ((LF-PARENT ONT::chart)
     (EXAMPLE "in a pie chart, the wedges represent the data series")
     (meta-data :origin task-learning :entry-date 20050816 :change-date nil :comments nil)
     )
    )
   )
))

