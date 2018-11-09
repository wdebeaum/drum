;;;;
;;;; W::MONDAY
;;;;

(define-words :pos W::name
 :words (
;; e.g Mondays are good for him, the monday before last etc.
	 (W::MONDAY
	  (senses((LF-PARENT ONT::DAY-NAME)
		  (TEMPL PPWORD-N-TEMPL)))
	  )
	 
	 (W::Mondays  ; plural only
	  (wordfeats (W::morph (:forms (-none))))
	  (SENSES
	   ((LF-PARENT ONT::DAY-NAME)
	    (TEMPL COUNT-PRED-3p-TEMPL)
	    )
	   )
	  )
	 ))

