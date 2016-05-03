;;;;
;;;; W::NOONE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::NOONE
   (wordfeats (W::ELSE-WORD +))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL PRONOUN-QUAN-TEMPL)
     )
    )
   )
))

