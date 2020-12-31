;;;;
;;;; W::suspend
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	 (W::suspend
	  (wordfeats (W::morph (:forms (-vb) :nom W::suspension)))
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put_spatial-9.2-1"))
	    (LF-PARENT ONT::hang-suspend-dangle) ;be-at-loc)
	    (TEMPL neutral-templ) ; like hang,stand,sit
	    )
	   ((LF-PARENT ONT::stop)
	    (TEMPL agent-affected-xp-np-templ) 
	   )
	  )
	  ))
 )

