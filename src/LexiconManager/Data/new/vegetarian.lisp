;;;;
;;;; w::vegetarian
;;;;

(define-words :pos W::n
 :words (
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Nouns added for FoodKB from Ben Snider
  (w::vegetarian
  (senses
	   ((LF-PARENT ONT::eater)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::VEGETARIAN W::VEGETABLE)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

