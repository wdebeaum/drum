;;;;
;;;; W::ONESELF
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::ONESELF
   (wordfeats (W::stem W::him) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     )
    )
   )
))

