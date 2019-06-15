;;;;
;;;; w::drain
;;;;

(define-words :pos W::V 
  :words (
	  (w::drain
	   (senses
	    ((lf-parent ont::take-incrementally)
	     (example "Drain the water from the container")
	     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
	     (meta-data :origin bee :entry-date 20040614 :change-date 20090529 :comments portability-experiment :vn ("clear-10.3-1"))
	     )
	    ((lf-parent ont::take-incrementally)
	     (example "the water drained from the container")
	     (templ affected-source-xp-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date 20090529 :comments portability-experiment)
	     )
	    ))
))

