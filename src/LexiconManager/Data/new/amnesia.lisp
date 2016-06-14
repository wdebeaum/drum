;;;;
;;;; w::amnesia
;;;;

(define-words :pos W::n
 :words (
  (w::amnesia
  (senses;;;;; names of medical conditions/symptoms that are mass nouns
   ;;;;; i.e., they can't take an indefinite article (*an arthritis) and they have no plural form
   ((meta-data :wn ("amnesia%1:09:00"))
    (LF-PARENT ONT::medical-symptom)
    (TEMPL MASS-PRED-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

