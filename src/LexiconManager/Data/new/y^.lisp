;;;;
;;;; W::Y^
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::Y^
   (wordfeats (W::lex W::you) (W::agr (? a W::2s W::2P)))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (LF-FORM W::YOU)
     )
    )
   )
))

