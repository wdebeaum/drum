;;;;
;;;; w::ignite
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   ;; require :theme to be (f::form f::object)
    (w::ignite
     (senses
      ((lf-parent ont::explode)
       (example "the compression ignites the fuel")
       (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
       (meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
       )
      ((lf-parent ont::explode)
	(example "the fuel ignites")
	(templ affected-templ)
	(meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)	
	)	    
      ))
))

