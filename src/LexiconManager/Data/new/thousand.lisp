;;;;
;;;; W::THOUSAND
;;;;

(define-words :pos W::n
 :words (
  ((W::THOUSAND W::ISLAND)
  (senses
	   ((LF-PARENT ONT::DRESSINGS-SAUCES-COATINGS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::NUMBER-UNIT :boost-word t :templ NUMBER-UNIT-TEMPL
 :tags (:base500)
 :words (
  (W::thousand
   (SENSES
    ((LF-PARENT ONT::NUMBER-UNIT)
     (SYNTAX (W::VAL 1000))
     )
    )
   )
))

