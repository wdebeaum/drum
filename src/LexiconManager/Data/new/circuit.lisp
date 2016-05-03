;;;;
;;;; w::circuit
;;;;

(define-words 
    :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;; have to define short circuit separately because of its morphology, and because it's a container (a circuit contains components)
	    (w::circuit
	     (senses 
	      ((LF-parent ONT::Device) 
	       (SEM (f::container +))
	       (templ count-pred-templ)
	       (meta-data :origin bee :entry-date 20040407 :change-date 20080716  :comments (test-s portability-followup beetle-pilots))
	       ))
	     )
))

