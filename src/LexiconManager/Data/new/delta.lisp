;;;;
;;;; w::delta
;;;;

(define-words :pos W::value :boost-word t
 :words (
  (w::delta
  (senses
   ((lf-parent ont::greek-letter-symbol)
    (templ value-templ)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (w::delta
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

