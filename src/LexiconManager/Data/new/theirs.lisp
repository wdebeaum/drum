;;;;
;;;; W::THEIRS
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
   (W::THEIRS
   (wordfeats (W::agr W::3p) (W::stem W::they))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL poss-pronoun-templ)
     (example "it is theirs")
     )
    )
   )
))

