;;;;
;;;; W::steady
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::steady
     (wordfeats (W::MORPH (:FORMS (-er -LY))))
   (SENSES
    ((LF-PARENT ONT::steady)
     (meta-data :origin cardiac :entry-date 20080508 :change-date 20090818 :comments LM-vocab)
     )
    ((LF-PARENT ONT::CONTINUOUS-VAL)
     (meta-data :origin chf :entry-date 20070810 :change-date nil :comments chf-dialogues)
     (EXAMPLE "I think my weight is steady")
     )
    )
   )
))

