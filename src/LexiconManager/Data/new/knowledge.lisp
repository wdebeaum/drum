;;;;
;;;; w::knowledge
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::knowledge
	   (senses
	    ((lf-parent ont::know)
	     (example "the knowledge of the subject")
	     (templ other-reln-theme-templ)
	     (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("knowledge%1:03:00") :comments portability-experiment)
	     )
	    ))
))

