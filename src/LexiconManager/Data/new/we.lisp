;;;;
;;;; W::WE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WE
   (wordfeats (W::agr W::1p) (W::CASE W::SUB))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (templ pronoun-plural-templ)
     )
    )
   )
))

