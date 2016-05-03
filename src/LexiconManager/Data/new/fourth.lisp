;;;;
;;;; W::fourth
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::fourth
   (SENSES
    ((LF (W::NTH 4))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype(? nt w::fraction w::day)))
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::fourth
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL PRED-S-TEMPL)
     )
    )
   )
))
