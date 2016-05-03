;;;;
;;;; w::another
;;;;


;;NB::  there are no entries for ANOTHER now -- it is handled in the grammar


#||(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
 (w::another
  (wordfeats (W::agr W::3s))
  (SENSES
   ((LF-PARENT ONT::REFERENTIAL-SEM)
    (TEMPL pronoun-indef-templ)
    (example "they took another")
    )
   )
  )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::another
   (wordfeats (W::status W::indefinite) (W::MASS W::COUNT) (W::negatable +))
   (SENSES
    ((LF W::other)
     (non-hierarchy-lf t)(TEMPL quan-sing-count-TEMPL)
     (SYNTAX (W::agr W::3s))
     )
    )
   )
))||#
