;;;;
;;;; W::ITSELF
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::ITSELF
   (wordfeats (W::stem W::it) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     )
    )
   )
))

