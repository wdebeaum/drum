;;;;
;;;; W::MEASURE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::MEASURE
   (SENSES
    #|
    (;(LF-PARENT ONT::domain)
     (LF-PARENT ONT::quantity)
     (TEMPL OTHER-RELN-TEMPL)
     (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050926 :CHANGE-DATE NIL :wn ("measure%1:03:00")
      :COMMENTS nil))
    |#
    
    ((LF-PARENT ONT::BAR-MEASURE)
     ;musical notation for a repeating pattern of musical beats
     (EXAMPLE "the orchestra omitted the last twelve measures of the song")
     (meta-data :wn ("measure%1:10:00"))
     (TEMPL other-reln-TEMPL)
     )
))
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

