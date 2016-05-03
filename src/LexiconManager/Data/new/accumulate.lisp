;;;;
;;;; W::accumulate
;;;;

(define-words :pos W::v 
 :words (
	 (W::accumulate
	  (wordfeats (W::morph (:forms (-vb) :nom w::accumulation)))
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090429 :comments nil :vn ("obtain-13.5.2") :wn ("?accumulate%2:40:00" "accumulate%2:35:00"))
	    (LF-PARENT ONT::amass)
	    (TEMPL agent-affected-source-templ (xp (% w::pp (w::ptype w::from)))) ; like recover
	    )
	   ((LF-PARENT ONT::amass)
	    (TEMPL affected-templ)
    )
   ))
  ))

