;;;;
;;;; w::boxing
;;;;

(define-words :pos W::n
 :words (
  (w::boxing
  (senses
   ((LF-PARENT ONT::sport) ;athletic-game)
    (TEMPL bare-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::name
 :words (
  ((w::boxing w::day)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

