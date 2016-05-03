;;;;
;;;; W::YOURSELF
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::YOURSELF
   (wordfeats (W::agr W::2S) (W::stem W::you) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     )
    )
   )
))

