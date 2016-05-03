;;;;
;;;; W::THEIR
;;;;

(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
	  (W::THEIR
	   (wordfeats (W::agr W::3p) (W::stem W::they))
	   (SENSES
	    ;; MD 11/11/2008 changed to referential-sem because "their terminals" can refer to "the bulbs' terminals"
	    ((LF-PARENT ONT::REFERENTIAL-SEM))
	    )
	   )
))

