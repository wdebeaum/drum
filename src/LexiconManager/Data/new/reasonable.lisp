;;;;
;;;; W::reasonable
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::reasonable
    (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("reasonable%5:00:00:rational:00" "reasonable%3:00:00" "reasonable%5:00:00:moderate:00") :comments caloy3)
     (EXAMPLE "it's a reasonable suggestion")
     (LF-PARENT ONT::reasonable-VAL)
     (TEMPL central-adj-content-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    ((meta-data :origin calo-ontology :entry-date 20060125 :change-date nil :wn ("reasonable%5:00:00:rational:00" "reasonable%3:00:00" "reasonable%5:00:00:moderate:00") :comments caloy3)
     (EXAMPLE "reasonable person")
     (LF-PARENT ONT::reasonable-VAL)
     (TEMPL central-adj-experiencer-TEMPL)
     (SEM (F::GRADABILITY F::+))
     )
    )
   )
))

