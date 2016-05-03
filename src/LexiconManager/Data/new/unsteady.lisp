;;;;
;;;; W::unsteady
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::unsteady
       (wordfeats (W::MORPH (:FORMS (-er -LY))))
   (SENSES
    ((LF-PARENT ONT::UNSTEADY)
     (meta-data :origin cardiac :entry-date 20080508 :change-date 20090731 :comments LM-vocab)
     )
    ((LF-PARENT ONT::CONTINUOUS-VAL)
     (meta-data :origin chf :entry-date 20090818 :change-date nil :comments chf-dialogues)
     (EXAMPLE "I think my weight is unsteady")
     )
    )
   )
))

