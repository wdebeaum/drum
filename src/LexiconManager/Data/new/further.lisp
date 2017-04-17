;;;;
;;;; W::further
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::further
   (SENSES
    ((LF-PARENT ONT::remaining-val)
     (example "there are no further problems")
     (SEM (F::GRADABILITY F::-))
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date nil :wn nil :comments caloy3)
     )
    ((LF-PARENT ONT::remote)
     (example "standing at further end of the clearing")
     (SEM (F::GRADABILITY F::-))
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date nil :wn ("further%5:00:00:far:00") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  (W::FURTHER
   (SENSES
    ((LF-PARENT ONT::EXTENSION)
     (LF-FORM W::FURTHER)
     (example "move further")
     (TEMPL PRED-S-POST-TEMPL)
     )
    ((LF-PARENT ONT::degree-modifier)
     (LF-FORM W::FURTHER)
     (TEMPL ADJ-ADV-OPERATOR-TEMPL)
     (meta-data :origin fruitcarts :entry-date 20050111 :change-date nil :comments nil)
     (example "move it a little bit further down")
     )
    )
   )
))

