;;;;
;;;; w::malaria
;;;;

(define-words :pos W::n
 :words (
  (w::malaria
  (senses;;;;; names of medical conditions/symptoms that are mass nouns
   ;;;;; i.e., they can't take an indefinite article (*an arthritis) and they have no plural form
   ((meta-data :wn ("infection%1:26:00" "malaria%1:26:00"))
    (LF-PARENT ONT::infection)
    (TEMPL MASS-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

