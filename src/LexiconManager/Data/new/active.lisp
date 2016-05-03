;;;;
;;;; W::ACTIVE
;;;;

(define-words :pos W::n
 :words (
  ((W::ACTIVE W::DRY)
  (senses
	   ((LF-PARENT ONT::food-prep-PROCESS)
	    (syntax (W::morph (:forms (-none))))
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::ACTIVE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::active)
     (SEM (F::GRADABILITY F::+))
     (TEMPL postpositive-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil)
     (LF-PARENT ONT::active)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

