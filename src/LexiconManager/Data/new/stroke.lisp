;;;;
;;;; W::stroke
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ;; reclassifying this from the more specific ont::pathology
 (W::stroke
   (SENSES
    ((meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     (LF-PARENT ONT::EVENT)
     (example "heat stroke" "piston stroke")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::stroke
  (senses;;;;; names of diseases/conditions that are count nouns and cannot appear without an article
	   ((meta-data :wn ("stroke%1:26:00"))
           (LF-PARENT ONT::medical-condition)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))

