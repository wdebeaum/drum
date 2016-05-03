;;;;
;;;; W::jul
;;;;

(define-words :pos W::name :boost-word t
 :words (
  (W::jul
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
  ((W::jul w::punc-period)
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

