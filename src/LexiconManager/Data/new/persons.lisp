;;;;
;;;; W::PERSONS
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PERSONS
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (LF-FORM W::PERSON)
     (TEMPL COUNT-PRED-3p-TEMPL)
     )
    )
   )
))

