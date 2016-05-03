;;;;
;;;; W::dollar-sign
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::dollar-sign
    (wordfeats (W::morph (:forms (-s-3p) :plur W::dollar-sign)))
   (SENSES
    ((LF-PARENT ONT::money-unit) (LF-FORM W::dollar)
     (meta-data :origin james :entry-date 20041206 :change-date nil)
     (example "I paid $100")
     (SYNTAX (W::punc +))
     (TEMPL pre-UNIT-TEMPL))))
))

