;;;;
;;;; W::weld
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
	 (W::weld
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
	    (LF-PARENT ONT::attach)
	    (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::to)))) ; like bind,glom,graft,bond,fasten,moor,bundle
	    )
	  	    )
	   )
))

