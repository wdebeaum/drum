;;;;
;;;; W::ANYTHING
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::ANYTHING
   (wordfeats (W::ELSE-WORD +))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL PRONOUN-INDEF-TEMPL)
     )
    )
   )
))

