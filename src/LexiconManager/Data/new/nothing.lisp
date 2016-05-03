;;;;
;;;; W::NOTHING
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::NOTHING
   (wordfeats (W::ELSE-WORD +))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL PRONOUN-QUAN-TEMPL)
     )
    )
   )
))

