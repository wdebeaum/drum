;;;;
;;;; W::WHOM
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHOM
   (wordfeats (W::agr ?agr) (W::sing-lf-only +) (W::ELSE-WORD +) (W::case W::OBJ) (W::WH (? wh W::Q W::R 
     )))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL PRONOUN-WH-TEMPL)
     )
    )
   )
))

