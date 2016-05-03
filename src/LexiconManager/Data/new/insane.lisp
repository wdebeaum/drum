;;;;
;;;; W::insane
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::insane
     (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :wn ("insane%3:00:00" "insane%5:00:00:foolish:00") :comments caloy3)
     (EXAMPLE "he's insane")
     (LF-PARENT ONT::insane)
     (TEMPL central-adj-experiencer-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090731 :comments caloy3)
     (EXAMPLE "that's insane")
     (LF-PARENT ONT::insane)
     (TEMPL central-adj-content-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

