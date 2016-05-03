;;;;
;;;; w::cavity
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	   (w::cavity
	   (senses
	    ((lf-parent ont::opening)
	     (example "the chest cavity")
	     (templ count-pred-templ)
	     (meta-data :origin cardiac :entry-date 20080414 :change-date nil :wn ("hole%1:17:01") :comments nil)
	     )
	    )
	   )
))

(define-words :pos w::N 
 :words (
;; adding synonyms for concepts in the joust demo to demonstrate lexical choice
  (w::cavity
  (senses((LF-parent ONT::hole) 
	    (templ count-pred-templ)
	    (example "there is a hole in front of the robot")
	    (meta-data :origin joust :entry-date 20091118 :change-date nil :comments joust-demo)
	    ))
)
))

