;;;;
;;;; w::phobia
;;;;

(define-words :pos W::n
 :words (
  (w::phobia
  (senses;;;;; names of medical conditions/symptoms that are mass nouns
   ;;;;; i.e., they can't take an indefinite article (*an arthritis) and they have no plural form
   ((meta-data :wn ("phobia%1:26:00"))
    (LF-PARENT ONT::medical-condition)
    (TEMPL MASS-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

