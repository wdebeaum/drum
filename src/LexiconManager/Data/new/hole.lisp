;;;;
;;;; w::hole
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::hole
	   (senses
	    ((lf-parent ont::opening)
	     (example "a hole in the ozone layer")
	     (templ count-pred-templ)
	     (meta-data :origin plow :entry-date 20060802 :change-date nil :wn ("hole%1:17:01") :comments weather)
	     )
	    ((lf-parent ont::hole)
	     (example "a hole in the ground") ;; can't go through it, so not an opening
	     (templ count-pred-templ)
	     (prototypical-word t)
	     (meta-data :origin joust :entry-date 20091027 :change-date nil :wn ("crater%1:17:01") :comments nil)
	     )
	    )
	   )
))

