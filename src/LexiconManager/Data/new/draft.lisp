;;;;
;;;; w::draft
;;;;

(define-words :pos W::n
 :words (
  ((w::draft w::beer)
  (senses
	   ((LF-PARENT ONT::ALCOHOL-COCKTAILS) ;ALCOHOL)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((w::draft w::beer)
  (senses
	   ((LF-PARENT ONT::ALCOHOL-COCKTAILS) ;ALCOHOL)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::draft
   (SENSES
    ((LF-PARENT ONT::version)
     (EXAMPLE "delete the draft")
     (meta-data :origin task-learning :entry-date 20050923 :change-date nil :wn ("draft%1:06:01" "draft%1:10:00") :comments nil)
     )
    )
   )
))

