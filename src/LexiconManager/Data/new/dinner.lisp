;;;;
;;;; W::DINNER
;;;;

(define-words :pos W::n
 :words (
  ((W::DINNER W::ROLL)
  (senses
	   ((LF-PARENT ONT::BAGELS-BISCUITS)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (W::DINNER
  (senses;;;;; meals may or may not have articles
   ((LF-PARENT ONT::meal-event)    
    (TEMPL BARE-PRED-TEMPL)
    )
   ((LF-PARENT ONT::food)
    (TEMPL BARE-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((w::dinner w::time)
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::recurring-time-of-day)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

