;;;;
;;;; W::HIS
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::HIS
   (wordfeats (W::agr w::3s) (W::stem W::he))
   (SENSES
    ((LF-PARENT ONT::MALE-PERSON)
     (TEMPL poss-pronoun-templ)
     (example "it is his")
     )
    )
   )
))

(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
	   (W::HIS
	   (wordfeats (W::agr W::3s) (W::stem W::he))
	   (SENSES
	    ((LF-PARENT ONT::MALE-PERSON))
	    )
	   )
))

(define-words :pos W::name
 :words (
  ((W::his W::highness)
  (senses((LF-PARENT ONT::title)
	    (syntax (w::title +))
	    (templ nname-templ)
	    )
	   )
)
))

