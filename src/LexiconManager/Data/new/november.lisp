;;;;
;;;; W::NOVEMBER
;;;;

(define-words :pos W::name :boost-word t
 :words (
  (W::NOVEMBER
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (w::november
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

