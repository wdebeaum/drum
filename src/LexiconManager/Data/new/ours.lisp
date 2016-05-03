;;;;
;;;; W::OURS
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
   (W::OURS
   (wordfeats (W::agr W::1p) (W::stem W::we))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL poss-pronoun-templ)
     (example "it is ours")
     )
    )
   )
))

