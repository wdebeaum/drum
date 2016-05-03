;;;;
;;;; W::WHO
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHO
   (wordfeats (W::sing-lf-only +) (W::agr ?agr) (W::ELSE-WORD +) (W::WH (? wh W::Q W::R)))
   (SENSES
    ((LF-PARENT ONT::person)
     (TEMPL PRONOUN-WH-TEMPL)
     )
    )
   )
))

