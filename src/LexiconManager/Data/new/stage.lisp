;;;;
;;;; w::stage
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::stage
	   (senses
	    ((lf-parent ont::phase)
	     (example "This stage of the plan")
	     (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("stage%1:26:00" "stage%1:28:00") :comments portability-experiment)
	     )	   	   	   	   
	    ))
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::stage
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("create-26.4-1"))
     (LF-PARENT ONT::arranging)
     )
    )
   )
))

