;;;;
;;;; W::wow
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
	  (W::wow
	   (SENSES
	    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("wow%2:37:00"))
	     (LF-PARENT ONT::impress)
	     (TEMPL agent-affected-xp-templ)
	     )
	    )
	   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::wow
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))

