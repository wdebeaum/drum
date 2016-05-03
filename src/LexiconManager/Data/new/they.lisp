;;;;
;;;; W::THEY
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THEY
   (wordfeats (W::agr W::3P) (W::CASE W::SUB))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (templ pronoun-plural-templ)
     )
    )
   )
))

