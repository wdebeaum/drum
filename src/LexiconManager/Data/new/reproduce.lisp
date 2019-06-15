;;;;
;;;; w::reproduce
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::reproduce
	   (wordfeats (W::morph (:forms (-vb) :nom W::reproduction)))
	   (senses
	    (
	     (lf-parent ont::cause-produce-reproduce) ;;  20120524 GUM change new parent
	     (example "reproduce the effect")
	     (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	   	   
	    	    ))
))

(define-words :pos W::v 
 :words (
  (W::reproduce
   (SENSES
    ((LF-PARENT ONT::procreate)
     (TEMPL agent-templ)
     (EXAMPLE "This species reproduce prolifically")
     )
    )
   )
))
