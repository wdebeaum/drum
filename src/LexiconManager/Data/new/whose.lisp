;;;;
;;;; W::WHOSE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHOSE
   (wordfeats (W::agr ?agr) (w::case (? cas w::sub w::obj -)) (W::sing-lf-only +))
   (SENSES
    ((LF-PARENT ONT::person)
     (example "whose")
     (SYNTAX (W::wh (? wh W::Q)))
     (TEMPL poss-pronoun-templ)
     (preference .97) ; prefer det sense: whose dog
     )
    )
   )
))

#|
(define-words :pos W::pro :boost-word t :templ poss-pro-det-templ
 :tags (:base500)
 :words (
	   (W::whose
	    (wordfeats (W::agr (? ag W::3s w::3p)) (W::stem W::who))
	   (SENSES
	    ((LF-PARENT ONT::referential-sem)
	     (example "the man whose dog barked" "the table whose legs wobble")
	     )
	     )
	    )
))
|#

(define-words :pos W::pro :boost-word t
 :tags (:base500)
 :words (
  (W::WHOSE
   (wordfeats (W::agr (? agr w::3s W::3p)) (W::mass (? mass W::COUNT W::MASS)) (W::WH (? wh W::Q W::R 
     )))
   (SENSES
    ((LF W::whose)
     (example "The man whose dog barked; Whose dog is this?")
     (non-hierarchy-lf t)
     (templ poss-pro-det-indef-templ) ;(TEMPL wh-qtype-TEMPL)
     )
    )
   )
))
