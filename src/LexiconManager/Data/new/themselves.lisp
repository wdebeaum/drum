;;;;
;;;; W::THEMSELVES
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THEMSELVES
   (wordfeats (W::AGR W::3P) (W::REFL +) (W::CASE W::OBJ))
   (SENSES
    ;; why isn't this ont::person?
    ;; doesn't have to refer to a person. The books themselves survived the flood, but the shelves were swept away
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (templ pronoun-plural-templ)
     )
    )
   )
))

