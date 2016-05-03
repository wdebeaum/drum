;;;;
;;;; W::OURSELVES
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::OURSELVES
   (wordfeats (W::agr W::1p) (W::stem W::we) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (templ pronoun-plural-templ)
     )
    )
   )
))

