;;;;
;;;; W::ANYBODY
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
 (W::ANYBODY
  (wordfeats (ELSE-WORD +))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL PRONOUN-INDEF-TEMPL)
     )
    )
   )
))

