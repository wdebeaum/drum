;;;;
;;;; W::fifth
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::fifth
   (SENSES
    ((LF (W::NTH 5))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype(? nt w::fraction w::day)))
     )
    )
   )
))

