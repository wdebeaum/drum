;;;;
;;;; W::MEASURE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::MEASURE
   (SENSES
    ((LF-PARENT ONT::domain)
     (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050926 :CHANGE-DATE NIL :wn ("measure%1:03:00")
      :COMMENTS nil))))
))

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
	  (w::measure
	   (senses
	    ((lf-parent ont::register)
	     (templ agent-templ)
	     (example "Slightly strange, but in the bee lexicon: You measured yesterday")
	     (preference 0.96)
	     )
	    ((lf-parent ont::register) 
	     (templ agent-neutral-templ))
	    
	    ))
))

