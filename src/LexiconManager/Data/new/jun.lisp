;;;;
;;;; W::jun
;;;;

(define-words :pos W::name :boost-word t
 :words (
  (W::jun
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
  ((W::jun w::punc-period)
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

