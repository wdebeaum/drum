;;;;
;;;; W::YOURSELVES
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::YOURSELVES
   (wordfeats (W::agr W::2p) (W::stem W::you) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (templ pronoun-plural-templ)
     )
    )
   )
))

