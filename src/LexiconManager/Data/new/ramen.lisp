;;;;
;;;; W::RAMEN
;;;;

(define-words :pos W::n
 :words (
  ((W::RAMEN W::NOODLE)
  (senses
	   ((LF-PARENT ONT::SOUP)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
  (w::ramen
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050801 :change-date nil :comments nil)
     (LF-PARENT ont::pasta)
     )
    )
   )
))

