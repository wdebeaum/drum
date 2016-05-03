;;;;
;;;; W::seventh
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::seventh
   (SENSES
    ((LF (W::NTH 7))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype(? nt w::fraction w::day)))
     )
    )
   )
))

