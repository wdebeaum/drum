;;;;
;;;; W::THEM
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THEM
   (wordfeats (W::agr W::3P) (w::case w::obj))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (templ pronoun-plural-templ)
     )
    )
   )
))

