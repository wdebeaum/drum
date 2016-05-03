;;;;
;;;; W::ANYONE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::ANYONE
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL PRONOUN-INDEF-TEMPL)
     (SYNTAX (W::ELSE-WORD +))
     )
    )
   )
))

