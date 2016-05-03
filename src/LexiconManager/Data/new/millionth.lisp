;;;;
;;;; W::millionth
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
   (W::millionth
   (SENSES
    ((LF (W::NTH 1000000))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype w::fraction))
     )
    )
   )
))

