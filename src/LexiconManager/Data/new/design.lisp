;;;;
;;;; w::design
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (w::design
	   (senses
	    ((lf-parent ont::invention)
	     (example "He designed a device")
	     (templ agent-affected-create-templ)
	     (meta-data :origin bee :entry-date 20040609 :change-date nil :comments portability-experiment :vn ("create-26.4-1"))
	     )
	    ))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PLAN
   (SENSES
    ((meta-data :origin domain-reorganization :entry-date 20170904 :change-date nil :comments nil :wn ("design%1:09:01"))
     (EXAMPLE "designs for world domination")
     (LF-PARENT ONT::mental-plan)
     )
    ))
))
