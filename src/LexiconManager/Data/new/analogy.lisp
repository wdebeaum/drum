;;;;
;;;; w::analogy
;;;;

(define-words :pos w::N 
  :templ other-reln-templ
 :words (
	  (w::analogy
	   (senses
	    ((lf-parent ont::compare) ;mental-object)
	     (templ other-reln-templ (xp (% w::pp (w::ptype (? wptp w::to w::between)))))
	     (meta-data :origin bee :entry-date 20040615 :change-date nil :wn ("analogy%1:04:00") :comments portability-experiment)
	     )	   	   	   	   
	    ))
))

