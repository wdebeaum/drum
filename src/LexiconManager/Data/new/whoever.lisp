;;;;
;;;; W::WHOEVER
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHOEVER
   (wordfeats (W::sing-lf-only +) (W::agr ?agr) (W::ELSE-WORD +) (W::WH (? wh W::Q W::R)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s14)
     (LF-PARENT ONT::PERSON)
     (TEMPL PRONOUN-WH-TEMPL)
     )
    )
   )
))

