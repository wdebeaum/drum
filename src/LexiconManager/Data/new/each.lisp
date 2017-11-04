;;;;
;;;; W::each
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :words (
  ((W::each w::other)
   (wordfeats (W::agr W::3p) (W::CASE W::OBJ))
   (SENSES
    ((LF-PARENT ONT::REFERENTIAL-SEM)
     (TEMPL pronoun-reciprocal-templ)
     (meta-data :origin csli-ts :entry-date 20070322 :change-date 20081111 :comments nil)
     (example "they evaluated each other" "they evaluated each other's projects" "the terminals are connected to each other")
     )
    )
   )
))

(define-words :pos W::quan :boost-word t
 :tags (:base500)
 :words (
  (W::EACH
   (wordfeats (W::status ont::quantifier) (W::mass W::count) (W::agr W::3s))
   (SENSES
    ((LF ONT::EACH)
     (non-hierarchy-lf t)(TEMPL quan-sing-count-TEMPL)
     )
    )
   )
))

