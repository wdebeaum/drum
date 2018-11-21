;;;;
;;;; W::MONDAY
;;;;

(define-words :pos W::name
 :words (
;; e.g Mondays are good for him, the monday before last etc.
	 (W::MONDAY
	  (wordfeats (w::sort time))
	  (senses((LF-PARENT ONT::DAY-NAME)
		  (templ nname-templ))) ;;(TEMPL PPWORD-N-TEMPL)))
	  )
	 
	 (W::Mondays  ; plural only
	  (wordfeats (W::agr 3p) (w::sort time) (W::morph (:forms (-none))))
	  (SENSES
	   ((LF-PARENT ONT::DAY-NAME)
	    (TEMPL NNAME-TEMPL)
	    )
	   )
	  )
	 ))

