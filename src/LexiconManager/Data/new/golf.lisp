;;;;
;;;; w::golf
;;;;

(define-words :pos W::value :boost-word t
 :words (
  (w::golf
  (senses
   ((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

(define-words :pos W::n
 :words (
  (w::golf
  (senses
   ((LF-PARENT ONT::athletic-game)
    (TEMPL bare-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )

   )
)
))

