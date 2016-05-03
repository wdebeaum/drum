;;;;
;;;; W::HERS
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
   (W::HERS
    (wordfeats (W::agr w::3s) (W::stem W::she))
    (SENSES
     ((LF-PARENT ONT::PERSON)
      (TEMPL poss-pronoun-templ)
      (example "it is hers")
      )
     )
    )
))

