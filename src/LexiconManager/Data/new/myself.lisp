;;;;
;;;; W::MYSELF
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::MYSELF
   (wordfeats (W::agr W::1S) (W::stem W::Me) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     )
    )
   )
))

