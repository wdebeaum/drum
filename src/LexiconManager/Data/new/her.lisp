;;;;
;;;; W::HER
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::HER
   (SENSES
     ((LF-PARENT ONT::FEMALE-PERSON)
     (SYNTAX (W::CASE W::OBJ))
     )
    )
   )
))

(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
	  (W::HER
	   (wordfeats (W::agr W::3s) (W::stem W::she))
	   (SENSES
	    ((LF-PARENT ONT::FEMALE-PERSON))
	    )
	   )
))

(define-words :pos W::name
 :words (
  ((W::her W::highness)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

