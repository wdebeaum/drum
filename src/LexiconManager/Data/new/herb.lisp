;;;;
;;;; w::herb
;;;;

(define-words :pos W::n
 :words (
  (w::herb
  (senses
	   ((LF-PARENT ONT::SPICES-HERBS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::HERB W::TEA)
  (senses
	   ((LF-PARENT ONT::TEAS) ;ONT::TEAS-BLENDS) ;TEAS-COCKTAILS-BLENDS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::HERB W::TEA)
  (senses
	   ((LF-PARENT ONT::TEAS) ;ONT::TEAS-BLENDS) ;TEAS-COCKTAILS-BLENDS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
  ;; moving w::tea to ont::tea for CAET
)
))

