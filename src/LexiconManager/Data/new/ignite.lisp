;;;;
;;;; w::ignite
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ;; require :theme to be (f::form f::object)
    (w::ignite
     (senses
      ((lf-parent ont::explode)
       (example "the compression ignites the fuel")
       (templ agent-affected-xp-templ)
       (meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
       )
      ((lf-parent ont::explode)
	(example "the fuel ignites")
	(templ affected-templ)
	(meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)	
	)	    
      ))
))

