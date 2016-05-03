;;;;
;;;; W::sep
;;;;

(define-words :pos W::name :boost-word t
 :words (
  (W::sep
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
  ((W::sep w::punc-period)
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

