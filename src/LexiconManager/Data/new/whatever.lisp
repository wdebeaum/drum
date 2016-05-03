;;;;
;;;; W::WHATEVER
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::WHATEVER
   (wordfeats (W::WH W::Q) (W::sing-lf-only +))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL PRONOUN-WH-TEMPL)
     )
    )
   )
))

