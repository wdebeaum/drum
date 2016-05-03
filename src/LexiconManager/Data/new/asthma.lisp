;;;;
;;;; w::asthma
;;;;

(define-words :pos W::n
 :words (
  (w::asthma
  (senses;;;;; names of medical conditions/symptoms that are mass nouns
   ;;;;; i.e., they can't take an indefinite article (*an arthritis) and they have no plural form
   ((LF-PARENT ONT::medical-disorders-and-conditions)
    (TEMPL MASS-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

