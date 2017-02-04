;;;;
;;;; W::SOUR
;;;;

(define-words :pos W::n
 :words (
  ((W::SOUR W::CREAM)
  (senses
	   ((LF-PARENT ONT::MILK)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::SOUR W::CREAM)
  (senses
	   ((LF-PARENT ONT::DRESSINGS-SAUCES-COATINGS)
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (w::sour
   (wordfeats (W::MORPH (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (LF-PARENT ONT::sour-val)
     )
    )
   )
))

