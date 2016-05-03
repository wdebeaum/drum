;;;;
;;;; W::US
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::US
   (wordfeats (W::agr W::1p) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (templ pronoun-plural-templ)
     )
    )
   )
))

