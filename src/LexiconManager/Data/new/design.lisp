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

