;;;;
;;;; W::insist
;;;;

(define-words :pos W::v 
 :words (
	 (W::insist
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("say-37.7"))
	    (LF-PARENT ONT::request)
	    (TEMPL agent-effect-xp-templ (xp (% w::cp (w::ctype w::s-that-subjunctive)))) ; like disclose
	    ) 
	   )
	  )

					; he insisted on coming
	 ((W::insist w::on)
	  (senses
	   ;;((LF-PARENT ONT::request)
	   ;; (TEMPL agent-formal-xp-templ (xp (% w::cp (w::ctype w::s-ing)))))
	   ((LF-PARENT ONT::request)
	    (TEMPL agent-neutral-xp-templ)
	    
	    ))
	  )
	 ))

