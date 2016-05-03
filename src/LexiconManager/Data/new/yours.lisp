;;;;
;;;; W::YOURS
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
   (W::YOURS
   (wordfeats (W::agr (? ag W::2s w::2p)) (W::stem W::you))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL poss-pronoun-templ)
     (example "it is yours")
     )
    )
   )
))

