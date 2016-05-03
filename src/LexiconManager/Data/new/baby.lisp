;;;;
;;;; w::baby
;;;;

(define-words :pos W::n
 :words (
  (w::baby
  (senses
   ((LF-PARENT ONT::child)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((w::baby W::FORMULA)
  (senses
	   ((LF-PARENT ONT::MEALS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
;; type of located event -- you can 'go to' them
  ((w::baby w::shower)
  (senses
   ((LF-PARENT ONT::gathering-event)
    (meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

