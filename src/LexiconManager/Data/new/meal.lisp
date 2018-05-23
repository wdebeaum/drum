;;;;
;;;; W::meal
;;;;

(define-words :pos W::n :templ count-pred-templ
 :words (
;           ))
	  (W::meal
	  (SENSES
	   ((LF-PARENT ONT::meal-event)
	    (example "take your medication after every meal")
	    (meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::food)
	    (example "he ate two meals a day")
	    (meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil :wn nil)
	    )	   
	   ))
))

(define-words :pos W::n
 :words (
  ((w::meal w::time)
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::recurring-time-of-day)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

