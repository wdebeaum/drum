;;;;
;;;; W::ITS
;;;;

(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
	  (W::ITS
	   (wordfeats (W::stem W::it))
	   (SENSES
	    ((LF-PARENT ONT::REFERENTIAL-SEM)
	     )
	    )
	   )
	  ((w::it w::^S)  ;; added for rebustness as this is a common error
	   (wordfeats (W::stem W::it))
	   (SENSES
	    ((LF-PARENT ONT::REFERENTIAL-SEM)
	     (preference .97)
	     )
	    ))
))

