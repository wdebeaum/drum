;;;;
;;;; w::window
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::window
	   (senses
	    ((lf-parent ont::display) ;; this is the window on the computer screen
	     (example "a window on the computer screen")
	     (templ count-pred-templ)
	     (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("window%1:06:03") :comments portability-experiment)
	     )
	    ((lf-parent ont::structural-opening)
	     (example "close the window shades")
	     (templ count-pred-templ)
	     (meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("window%1:06:00") :comments projector-purchasing)
	     )	 
	    ))
))

