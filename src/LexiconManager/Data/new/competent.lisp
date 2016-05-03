;;;;
;;;; W::competent
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::competent
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :wn ("competent%3:00:00") :comments caloy3)
     (EXAMPLE "He is an competent person")
     (LF-PARENT ONT::ability-val)
     (TEMPL central-adj-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

