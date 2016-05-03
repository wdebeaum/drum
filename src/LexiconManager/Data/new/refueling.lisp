;;;;
;;;; W::REFUELING
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::REFUELING W::PORT)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::refueling W::ports))))
   (SENSES
    ((LF-PARENT ONT::transportation-facility)
     (LF-FORM W::refueling_port)
     )
    )
   )
))

