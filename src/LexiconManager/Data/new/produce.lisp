;;;;
;;;; w::produce
;;;;

(define-words :pos W::V 
  :templ agent-affected-create-templ
 :tags (:base500)
 :words (
	  (w::produce
	   (wordfeats (W::morph (:forms (-vb) :nom W::production)))
	   (senses
	    (
	     (lf-parent ont::cause-produce-reproduce) ;;  20120524 GUM change new parent
	     (example "produce the effect")
	     (templ agent-affected-create-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	   	   
	    #||((lf-parent ont::create)
	     (example "produce a circuit")
	     (meta-data :origin bee :entry-date 20040805 :change-date nil :comments portability-experiment)
	     )	||#   	   
	    ))
))

