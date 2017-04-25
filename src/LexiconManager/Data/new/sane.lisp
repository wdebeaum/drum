;;;;
;;;; W::sane
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::sane
     (wordfeats (W::morph (:FORMS (-er -LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("sane%5:00:00:rational:00" "sane%3:00:00") :comments caloy3)
     (EXAMPLE "he is a sane person")
     (lf-parent ont::sensible-val)
     (TEMPL central-adj-experiencer-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :comments caloy3)
     (EXAMPLE "reasonable idea")
     (lf-parent ont::sensible-val)
     (TEMPL central-adj-content-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

