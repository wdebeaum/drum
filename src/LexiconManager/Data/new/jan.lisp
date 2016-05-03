;;;;
;;;; w::jan
;;;;

(define-words :pos W::name :boost-word t
 :words (
  (w::jan
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

(define-words :pos W::name :boost-word t
 :words (
  ((w::jan w::punc-period)
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

