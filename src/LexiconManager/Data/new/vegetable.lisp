;;;;
;;;; W::VEGETABLE
;;;;

(define-words :pos W::n
 :words (
  ((W::VEGETABLE W::BEEF)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  (W::VEGETABLE
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

