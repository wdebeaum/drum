;;;;
;;;; W::^EM
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::^EM
   (wordfeats (W::agr W::3P) (w::case w::obj))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (LF-FORM W::THEM)
     (templ pronoun-plural-templ)
     )
    )
   )
))

