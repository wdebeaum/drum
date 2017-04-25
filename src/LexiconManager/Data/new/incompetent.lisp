;;;;
;;;; W::incompetent
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::incompetent
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :wn ("incompetent%3:00:00") :comments caloy3)
     (EXAMPLE "He is an incompetent person")
     (lf-parent ont::unable)
     (TEMPL less-adj-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

