;;;;
;;;; W::eighth
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::eighth
   (SENSES
    ((LF (W::NTH 8))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype(? nt w::fraction w::day)))
     )
    )
   )
))

