;;;;
;;;; W::WHOLE
;;;;

(define-words :pos W::n
 :words (
  ((W::WHOLE W::KERNEL W::CORN)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::WHOLE W::GROAT W::BUCKWHEAT W::FLOUR)
  (senses
	   ((LF-PARENT ONT::GRAINS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::WHOLE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("whole%3:00:00"))
     (LF-PARENT ONT::WHOLE-complete)
     )
    )
   )
))

