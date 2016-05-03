;;;;
;;;; w::reproduce
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (w::reproduce
	   (wordfeats (W::morph (:forms (-vb) :nom W::reproduction)))
	   (senses
	    (
	     (lf-parent ont::cause-produce-reproduce) ;;  20120524 GUM change new parent
	     (example "reproduce the effect")
	     (templ agent-affected-create-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	   	   
	    	    ))
))

