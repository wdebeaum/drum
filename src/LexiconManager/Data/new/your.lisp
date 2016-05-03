;;;;
;;;; W::YOUR
;;;;

(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
	   (W::YOUR
	   (wordfeats (W::agr (? ag W::2S W::2p)) (W::stem W::you))
	   (SENSES
	    ((LF-PARENT ONT::PERSON))
	    )
	   )
))

