;;;;
;;;; W::coffee
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
(W::coffee
   (SENSES
    ((meta-data :origin caet :entry-date 20111220)
     (LF-PARENT ONT::coffee)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::coffee W::shop)
   (SENSES
    ((LF-PARENT ONT::coffee-shop)
     (example "let me teach you how to find the nearest Starbucks coffee shop")
     (meta-data :origin sense :entry-date 20091005 :change-date nil :wn ("shop%1:06:00") :comments demo)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::coffee
  (senses
	   ((LF-PARENT ONT::BEVERAGES)
	    (TEMPL count-PRED-TEMPL)
	    (SEM (F::form F::liquid))
	    )
	   )
)
))

