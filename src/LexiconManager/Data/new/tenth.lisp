;;;;
;;;; W::tenth
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::tenth
   (SENSES
    ((LF (W::NTH 10))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype(? nt w::fraction w::day)))
     )
    )
   )
))

