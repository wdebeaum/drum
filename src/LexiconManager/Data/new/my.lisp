;;;;
;;;; W::MY
;;;;

(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
;; these must modify a noun e.g. my/your/his/her book -- unlike possessive pronouns that can stand alone: mine, yours
	  (W::MY
	   (wordfeats (W::agr W::1S) (W::stem W::Me))
	   (SENSES
	    ((LF-PARENT ONT::PERSON))
	    )
	   )
))

